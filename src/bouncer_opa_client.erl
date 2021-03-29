-module(bouncer_opa_client).

%% API Functions

-export([init/1]).
-export([request_document/3]).

%% API Types

-type endpoint() :: {resolve() | inet:hostname() | inet:ip_address(), inet:port_number()}.
-type resolve() :: {resolve, dns, inet:hostname(), #{pick => gunner_resolver:ip_picker()}}.

-type opts() :: #{
    pool_opts := gunner:pool_opts(),
    endpoint := endpoint(),
    request_timeout => timeout()
}.

-opaque client() :: #{
    endpoint := endpoint(),
    request_timeout := timeout(),
    connect_timeout := timeout()
}.

-type ruleset_id() :: iodata().

-type document() ::
    null
    | binary()
    | number()
    | boolean()
    | #{atom() | binary() => document()}
    | [document()].

-export_type([opts/0]).
-export_type([client/0]).
-export_type([ruleset_id/0]).
-export_type([document/0]).

%%

-define(DEFAULT_REQUEST_TIMEOUT, 1000).
-define(GUNNER_POOL_ID, bouncer_opa_client_pool).

%%
%% API Functions
%%

-spec init(opts()) -> {client(), supervisor:child_spec()}.
init(OpaClientOpts) ->
    PoolOpts = maps:get(pool_opts, OpaClientOpts),
    PoolReg = {local, ?GUNNER_POOL_ID},
    ChildSpec = #{
        id => ?GUNNER_POOL_ID,
        start => {gunner_pool, start_link, [PoolReg, PoolOpts]}
    },
    Client = genlib_map:compact(#{
        endpoint => maps:get(endpoint, OpaClientOpts),
        request_timeout => get_request_timeout(OpaClientOpts),
        connect_timeout => get_connect_timeout(PoolOpts)
    }),
    {Client, ChildSpec}.

-spec request_document(_ID :: iodata(), _Input :: document(), client()) ->
    {ok, document()}
    | {error,
        notfound
        | {unavailable, _Reason}
        | {unknown, _Reason}}.
request_document(RulesetID, Input, Client) ->
    #{
        endpoint := Endpoint,
        request_timeout := RequestTimeout
    } = Client,
    Path = join_path(<<"/v1/data">>, join_path(RulesetID, <<"/judgement">>)),
    % TODO
    % A bit hacky, ordsets are allowed in context and supposed to be opaque, at least by design.
    % We probably need something like `bouncer_context:to_json/1`.
    Body = jsx:encode(#{input => Input}),
    CType = <<"application/json; charset=utf-8">>,
    Headers = #{
        <<"content-type">> => CType,
        <<"accept">> => CType
    },
    Deadline = erlang:monotonic_time(millisecond) + RequestTimeout,
    try
        ResolvedEndpoint = resolve_endpoint(Endpoint, RequestTimeout),
        TimeoutLeft = Deadline - erlang:monotonic_time(millisecond),
        GunnerOpts = make_gunner_opts(TimeoutLeft, Client),
        %% Trying the synchronous API first
        case gunner:post(?GUNNER_POOL_ID, ResolvedEndpoint, Path, Body, Headers, GunnerOpts) of
            {ok, 200, _, Response} when is_binary(Response) ->
                decode_document(Response);
            {ok, 404, _, _} ->
                {error, notfound};
            {ok, Code, _, Response} ->
                {error, {unknown, {Code, Response}}};
            {error, {unknown, Reason}} ->
                {error, {unknown, Reason}};
            {error, Reason} ->
                {error, {unavailable, Reason}}
        end
    catch
        throw:{resolve_failed, ResolvError} ->
            {error, {unavailable, ResolvError}}
    end.

%%

-spec decode_document(binary()) -> {ok, document()} | {error, notfound}.
decode_document(Response) ->
    case jsx:decode(Response) of
        #{<<"result">> := Result} ->
            {ok, Result};
        #{} ->
            {error, notfound}
    end.

%%

resolve_endpoint({{resolve, dns, Hostname, Opts}, Port}, Timeout) ->
    case gunner_resolver:resolve_endpoint({Hostname, Port}, make_resolver_opts(Timeout, Opts)) of
        {ok, ResolvedEndpoint} ->
            ResolvedEndpoint;
        {error, Reason} ->
            throw({resolve_failed, Reason})
    end;
resolve_endpoint(Endpoint, _Timeout) ->
    Endpoint.

make_resolver_opts(Timeout, #{pick := IpPicker}) ->
    #{timeout => Timeout, ip_picker => IpPicker}.

make_gunner_opts(RequestTimeout, #{connect_timeout := ConnectTimeout}) ->
    #{request_timeout => RequestTimeout, acquire_timeout => ConnectTimeout};
make_gunner_opts(RequestTimeout, _Client) ->
    #{request_timeout => RequestTimeout}.

-spec get_request_timeout(opts()) -> timeout().
get_request_timeout(Opts) ->
    maps:get(request_timeout, Opts, ?DEFAULT_REQUEST_TIMEOUT).

-spec get_connect_timeout(gunner:pool_opts()) -> timeout() | undefined.
get_connect_timeout(PoolOpts) ->
    ClientOpts = maps:get(connection_opts, PoolOpts, #{}),
    maps:get(connect_timeout, ClientOpts, undefined).

%%

join_path(F1, F2) when is_binary(F1), is_binary(F2) ->
    normalize_path(genlib_string:cat(normalize_path(F1), normalize_path(F2))).

normalize_path(P = <<$/, P1/binary>>) ->
    S1 = byte_size(P1),
    case S1 > 0 andalso binary:last(P1) of
        $/ -> binary:part(P, 0, S1);
        _ -> P
    end;
normalize_path(P) when is_binary(P) ->
    normalize_path(<<$/, P/binary>>);
normalize_path(P) ->
    normalize_path(iolist_to_binary(P)).
