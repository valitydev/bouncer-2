-module(ct_proxy).

-export([start_link/1]).
-export([start_link/2]).
-export([unlink/1]).
-export([endpoint/1]).
-export([mode/2]).
-export([mode/3]).
-export([stop/1]).

%%

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%

-include_lib("kernel/include/inet.hrl").

-type endpoint() :: {inet:hostname(), inet:port_number()}.
-type scope()    :: listen | connection.
-type mode()     :: ignore | stop | relay.
-type modes()    :: #{scope() => mode()}.

-type proxy() :: pid().

-spec start_link(endpoint()) ->
    {ok, proxy()}.

-spec start_link(endpoint(), modes()) ->
    {ok, proxy()}.

-spec start_link(endpoint(), modes(), ranch_tcp:opts()) ->
    {ok, proxy()}.

-spec unlink(proxy()) ->
    proxy().

start_link(Upstream) ->
    start_link(Upstream, #{}).

start_link(Upstream, Modes) ->
    start_link(Upstream, Modes, [{ip, {127, 0, 0, 1}}, {backlog, 1}]).

start_link(Upstream, Modes, SocketOpts) ->
    Args = {resolve_endpoint(Upstream), Modes, SocketOpts},
    gen_server:start_link(?MODULE, Args, []).

resolve_endpoint({Host, Port}) ->
    {ok, #hostent{h_addr_list = [Address | _Rest]}} = inet:gethostbyname(Host),
    {Address, Port}.

unlink(Proxy) when is_pid(Proxy) ->
    true = erlang:unlink(Proxy),
    Proxy.

-spec endpoint(proxy()) ->
    endpoint().
endpoint(Proxy) when is_pid(Proxy) ->
    gen_server:call(Proxy, endpoint).

-spec mode(proxy(), scope()) ->
    {mode(), _Upstream :: endpoint()}.

mode(Proxy, Scope) when is_pid(Proxy) ->
    gen_server:call(Proxy, {mode, Scope}).

-spec mode(proxy(), scope(), mode()) ->
    mode().

mode(Proxy, Scope, Mode) when is_pid(Proxy) ->
    gen_server:call(Proxy, {mode, Scope, Mode}).

-spec stop(proxy()) ->
    ok.

stop(Proxy) when is_pid(Proxy) ->
    proc_lib:stop(Proxy, shutdown).

%%

-record(st, {
    lsock     :: _Socket | undefined,
    lsockopts :: list(),
    acceptor  :: pid() | undefined,
    modes     :: #{scope() => mode()},
    upstream  :: {inet:ip_address(), inet:port_number()}
}).

-type st() :: #st{}.

-spec init(_) ->
    {ok, st()}.

init({Upstream, Modes0, SocketOpts}) ->
    Modes = maps:merge(#{listen => relay, connection => relay}, Modes0),
    St = #st{
        lsockopts = SocketOpts,
        modes = Modes,
        upstream = Upstream
    },
    {ok, sync_mode(listen, stop, maps:get(listen, Modes), St)}.

-spec handle_call(_Call, _From, st()) ->
    {noreply, st()}.

handle_call(endpoint, _From, St = #st{}) ->
    {reply, get_endpoint(St), St};
handle_call({mode, Scope, Mode}, _From, St = #st{modes = Modes}) ->
    ModeWas = maps:get(Scope, Modes),
    StNext = sync_mode(Scope, ModeWas, Mode, St),
    {reply, ModeWas, StNext#st{modes = Modes#{Scope := Mode}}};
handle_call({mode, Scope}, _From, St = #st{modes = Modes, upstream = Endpoint}) ->
    {reply, {maps:get(Scope, Modes), Endpoint}, St};
handle_call(_Call, _From, St) ->
    {noreply, St}.

-spec handle_cast(_Cast, st()) ->
    {noreply, st()}.

handle_cast(_Cast, St) ->
    {noreply, St}.

-spec handle_info(_Info, st()) ->
    {noreply, st()}.

handle_info(_Info, St) ->
    {noreply, St}.

-spec terminate(_Reason, st()) ->
    _.

terminate(_Reason, _St) ->
    ok.

-spec code_change(_Vsn | {down, _Vsn}, st(), _Extra) ->
    {ok, st()}.

code_change(_Vsn, St, _Extra) ->
    {ok, St}.

%%

get_endpoint(#st{lsock = undefined}) ->
    undefined;
get_endpoint(#st{lsock = LSock}) ->
    {ok, {IP, Port}} = ranch_tcp:sockname(LSock),
    {inet:ntoa(IP), Port}.

sync_mode(listen, Mode, Mode, St) ->
    St;
sync_mode(listen = Scope, stop, relay, St) ->
    St1 = sync_mode(Scope, stop, ignore, St),
    St2 = sync_mode(Scope, ignore, relay, St1),
    St2;
sync_mode(listen, stop, ignore, St) ->
    start_listener(St);
sync_mode(listen, ignore, relay, St) ->
    start_acceptor(St);
sync_mode(listen = Scope, relay, stop, St) ->
    St1 = sync_mode(Scope, relay, ignore, St),
    St2 = sync_mode(Scope, ignore, stop, St1),
    St2;
sync_mode(listen, relay, ignore, St) ->
    stop_acceptor(St);
sync_mode(listen, ignore, stop, St) ->
    stop_listener(St);
sync_mode(connection, _, _, St) ->
    St.

start_listener(St = #st{lsock = undefined, lsockopts = SocketOpts}) ->
    ct:pal("start_listener @ ~p", [St]),
    {ok, LSock} = ranch_tcp:listen(SocketOpts),
    St#st{lsock = LSock}.

stop_listener(St = #st{lsock = LSock}) when lsock /= undefined ->
    ct:pal("stop_listener @ ~p", [St]),
    ok = ranch_tcp:close(LSock),
    St#st{lsock = undefined}.

%%

start_acceptor(St = #st{acceptor = undefined, lsock = LSock}) ->
    ct:pal("start_acceptor @ ~p", [St]),
    Parent = self(),
    Pid = erlang:spawn_link(fun () -> loop_acceptor(Parent, LSock) end),
    St#st{acceptor = Pid}.

stop_acceptor(St = #st{acceptor = Pid}) when is_pid(Pid) ->
    ct:pal("stop_acceptor @ ~p", [St]),
    MRef = erlang:monitor(process, Pid),
    true = erlang:unlink(Pid),
    true = erlang:exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _Reason} ->
        St#st{acceptor = undefined}
    end.

loop_acceptor(Parent, LSock) ->
    _ = case ranch_tcp:accept(LSock, infinity) of
        {ok, CSock} ->
            _ = ct:pal("accepted ~p from ~p", [CSock, ranch_tcp:peername(CSock)]),
            _ = spawn_proxy_connection(Parent, CSock),
            loop_acceptor(Parent, LSock);
        {error, Reason} ->
            exit(Reason)
    end.

%%

-define(PROXY_RECV_TIMEOUT, 1000).
-define(PROXY_SOCKET_OPTS, [{packet, 0}, {active, once}]).

-record(proxy, {
    insock :: _Socket,
    upsock :: _Socket | undefined,
    parent :: pid(),
    upstream :: endpoint() | undefined,
    buffer = <<>> :: binary(),
    timeout = ?PROXY_RECV_TIMEOUT :: timeout()
}).

spawn_proxy_connection(Parent, CSock) ->
    ProxySt = #proxy{insock = CSock, parent = Parent},
    erlang:spawn_link(fun () -> loop_proxy_connection(ProxySt) end).

loop_proxy_connection(St = #proxy{insock = InSock, parent = Parent, buffer = Buffer}) ->
    case ranch_tcp:recv(InSock, 0, ?PROXY_RECV_TIMEOUT) of
        {ok, Data} ->
            Buffer1 = <<Buffer/binary, Data/binary>>,
            {Mode, Endpoint} = mode(Parent, connection),
            case Mode of
                stop ->
                    terminate(St);
                ignore ->
                    loop_proxy_connection(St);
                relay ->
                    loop_proxy_relay(St#proxy{buffer  = Buffer1, upstream = Endpoint})
            end;
        _ ->
            terminate(St)
    end.

loop_proxy_relay(St = #proxy{upsock = undefined, upstream = Endpoint, buffer = Buffer}) ->
    case remote_connect(Endpoint) of
        {ok, Socket} ->
            ok = ranch_tcp:send(Socket, Buffer),
            loop_proxy_relay(St#proxy{upsock = Socket, buffer = <<>>});
        {error, _Error} ->
            terminate(St)
    end;
loop_proxy_relay(St = #proxy{insock = InSock, upsock = UpSock}) ->
    ok = ranch_tcp:setopts(InSock, ?PROXY_SOCKET_OPTS),
    ok = ranch_tcp:setopts(InSock, ?PROXY_SOCKET_OPTS),
    receive
        {_, InSock, Data} ->
            ranch_tcp:send(UpSock, Data),
            loop_proxy_relay(St);
        {_, UpSock, Data} ->
            ranch_tcp:send(InSock, Data),
            loop_proxy_relay(St);
        {tcp_closed, UpSock} ->
            terminate(St);
        {tcp_closed, InSock} ->
            ranch_tcp:close(UpSock);
        _ ->
            _ = ranch_tcp:close(UpSock),
            terminate(St)
    end.

remote_connect({IP, Port}) ->
    gen_tcp:connect(IP, Port, [binary, {packet, 0}, {delay_send, true}]).

terminate(#proxy{insock = InSock}) ->
    ranch_tcp:close(InSock).
