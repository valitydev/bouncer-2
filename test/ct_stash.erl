-module(ct_stash).
-behaviour(gen_server).

-export([start/0]).
-export([destroy/1]).
-export([append/3]).
-export([flush/2]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(CALL_TIMEOUT, 1000).

%%% API

-type key() :: _.
-type entry() :: _.

-spec start() ->
    {ok, pid()}.
start() ->
    gen_server:start(?MODULE, [], []).

-spec destroy(pid()) ->
    ok | {error, {nonempty, _Left :: #{key() => entry()}}}.
destroy(Pid) ->
    call(Pid, destroy).

-spec append(pid(), key(), entry()) ->
    ok.
append(Pid, Key, Entry) ->
    call(Pid, {append, Key, Entry}).

-spec flush(pid(), key()) ->
    {ok, [entry()]} | error.
flush(Pid, Key) ->
    call(Pid, {flush, Key}).

call(Pid, Msg) ->
    gen_server:call(Pid, Msg, ?CALL_TIMEOUT).

%%% gen_server callbacks

-spec init(term()) ->
    {ok, atom()}.
init(_) ->
    {ok, #{}}.

-spec handle_call(term(), pid(), atom()) ->
    {reply, atom(), atom()}.
handle_call({append, Key, Entry}, _From, State) ->
    Entries = maps:get(Key, State, []),
    State1 = maps:put(Key, [Entry | Entries], State),
    {reply, ok, State1};
handle_call({flush, Key}, _From, State) ->
    case maps:take(Key, State) of
        {Entries, State1} ->
            {reply, {ok, lists:reverse(Entries)}, State1};
        error ->
            {reply, error, State}
    end;
handle_call(destroy, _From, State) ->
    case maps:size(State) of
        0 ->
            {stop, shutdown, ok, State};
        _ ->
            Left = maps:map(fun (_, Entries) -> lists:reverse(Entries) end, State),
            Reason = {error, {nonempty, Left}},
            {stop, Reason, Reason, State}
    end.

-spec handle_cast(term(), atom()) ->  {noreply, atom()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), atom()) -> {noreply, atom()}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), atom()) -> atom().

terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(), term()) -> {ok, atom()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
