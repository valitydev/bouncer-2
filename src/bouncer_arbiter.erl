-module(bouncer_arbiter).

%% TODOs
%% * Enable connection pooling, to lower IO resource usage.
%% * Respect external woody deadlines where applicable.

%% NOTE
%% This must be a path to some document with the subdocument of the following form:
%% ```
%% "assertions": {
%%   "allowed": [{"code": "...", ...}, ...] // 0 or more, may be unset
%%   "forbidden": [{"code": "...", ...}, ...] // 0 or more, may be unset
%% }
%% ```
-type ruleset_id() :: iodata().

-type judgement() :: {resolution(), [assertion()]}.
-type resolution() :: allowed | forbidden | {restricted, map()}.
-type assertion() :: {_Code :: binary(), _Details :: #{binary() => _}}.

-export_type([judgement/0]).
-export_type([resolution/0]).

-export([judge/2]).

%%

-spec judge(ruleset_id(), bouncer_context:ctx()) ->
    {ok, judgement()}
    | {error,
        ruleset_notfound
        | {ruleset_invalid, _Details}
        | {unavailable | unknown, _Reason}}.
judge(RulesetID, Context) ->
    case mk_opa_client() of
        {ok, Client} ->
            Location = join_path(RulesetID, <<"/judgement">>),
            case request_opa_document(Location, Context, Client) of
                {ok, Document} ->
                    infer_judgement(Document);
                {error, notfound} ->
                    {error, ruleset_notfound};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec infer_judgement(document()) -> {ok, judgement()} | {error, {ruleset_invalid, _Details}}.
infer_judgement(Document) ->
    case jesse:validate_with_schema(get_judgement_schema(), Document) of
        {ok, _} ->
            {ok, parse_judgement(Document)};
        {error, Reason} ->
            {error, {ruleset_invalid, Reason}}
    end.

parse_judgement(#{<<"resolution">> := [<<"forbidden">>, Assertions]}) ->
    {forbidden, extract_assertions(Assertions)};
parse_judgement(#{<<"resolution">> := [<<"allowed">>, Assertions]}) ->
    {allowed, extract_assertions(Assertions)};
parse_judgement(#{
    <<"resolution">> := [<<"restricted">>, Assertions],
    <<"restrictions">> := Restrictions
}) ->
    {{restricted, Restrictions}, extract_assertions(Assertions)}.

extract_assertions(Assertions) ->
    [extract_assertion(E) || E <- Assertions].

extract_assertion(Assertion = #{<<"code">> := Code}) ->
    {Code, maps:without([<<"code">>], Assertion)}.

-spec get_judgement_schema() -> jesse:schema().
get_judgement_schema() ->
    % TODO
    % Worth declaring in a separate file? Should be helpful w/ CI-like activities.
    AssertionsSchema = [
        {<<"type">>, <<"array">>},
        {<<"items">>, [
            {<<"type">>, <<"object">>},
            {<<"required">>, [<<"code">>]},
            {<<"properties">>, [
                {<<"code">>, [
                    {<<"type">>, <<"string">>},
                    {<<"minLength">>, 1}
                ]}
            ]}
        ]}
    ],
    ResolutionSchema = [
        {<<"type">>, <<"array">>},
        {<<"items">>, [
            [
                {<<"type">>, <<"string">>},
                {<<"pattern">>, <<"allowed|forbidden|restricted">>}
            ],
            AssertionsSchema
        ]},
        {<<"minItems">>, 2},
        {<<"additionalItems">>, false}
    ],
    [
        {<<"$schema">>, <<"http://json-schema.org/draft-04/schema#">>},
        {<<"type">>, <<"object">>},
        {<<"properties">>, [
            {<<"resolution">>, ResolutionSchema},
            {<<"restrictions">>, [
                {<<"type">>, <<"object">>}
            ]}
        ]},
        {<<"additionalProperties">>, false},
        {<<"required">>, [<<"resolution">>]}
    ].

%%

-type endpoint() :: {inet:hostname() | inet:ip_address(), inet:port_number()}.
-type client_opts() :: #{
    endpoint := endpoint(),
    transport => tcp | tls,
    tcp_opts => [gen_tcp:connect_option()],
    tls_opts => [ssl:tls_client_option()],
    connect_timeout => timeout(),
    domain_lookup_timeout => timeout(),
    request_timeout => timeout(),
    http_opts => gun:http_opts(),
    http2_opts => gun:http2_opts(),
    % TODO
    % Pulse over gun event handler mechanic.
    event_handler => {module(), _State}
}.

-define(DEFAULT_CLIENT_OPTS, #{
    domain_lookup_timeout => 1000,
    connect_timeout => 1000,
    request_timeout => 1000
}).

-type client() :: {pid(), client_opts()}.
-type document() ::
    null
    | binary()
    | number()
    | boolean()
    | #{atom() | binary() => document()}
    | [document()].

-spec mk_opa_client() -> {ok, client()} | {error, {unavailable, _Reason}}.
mk_opa_client() ->
    Opts = get_opa_client_opts(),
    {Host, Port} = maps:get(endpoint, Opts),
    GunOpts = maps:with(
        [
            transport,
            tcp_opts,
            tls_opts,
            connect_timeout,
            domain_lookup_timeout,
            http_opts,
            http2_opts,
            event_handler
        ],
        Opts
    ),
    % TODO
    % We might want to mask intermittent service unavailability here with retries though.
    % Leave it up to our clients for the time being, in the name of configuration simplicity
    % and predictability.
    case gun:open(Host, Port, GunOpts#{retry => 0}) of
        {ok, Client} ->
            Timeout = maps:get(connect_timeout, Opts),
            case gun:await_up(Client, Timeout) of
                {ok, _} ->
                    {ok, {Client, Opts}};
                {error, Reason} ->
                    {error, {unavailable, Reason}}
            end;
        {error, Reason = {options, _}} ->
            erlang:error({invalid_client_options, Reason, Opts})
    end.

-spec request_opa_document(_ID :: iodata(), _Input :: document(), client()) ->
    {ok, document()}
    | {error,
        notfound
        | {unknown, _Reason}}.
request_opa_document(ID, Input, {Client, Opts}) ->
    Path = join_path(<<"/v1/data">>, ID),
    % TODO
    % A bit hacky, ordsets are allowed in context and supposed to be opaque, at least by design.
    % We probably need something like `bouncer_context:to_json/1`.
    Body = jsx:encode(#{input => Input}),
    CType = <<"application/json; charset=utf-8">>,
    Headers = #{
        <<"content-type">> => CType,
        <<"accept">> => CType
    },
    Timeout = maps:get(request_timeout, Opts),
    StreamRef = gun:post(Client, Path, Headers, Body),
    % TODO think about implications
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    case gun:await(Client, StreamRef, Timeout) of
        {response, nofin, 200, _Headers} ->
            TimeoutLeft = Deadline - erlang:monotonic_time(millisecond),
            case gun:await_body(Client, StreamRef, TimeoutLeft) of
                {ok, Response, _Trailers} ->
                    decode_document(Response);
                {ok, Response} ->
                    decode_document(Response);
                {error, Reason} ->
                    {error, {unknown, Reason}}
            end;
        {response, fin, 404, _Headers} ->
            {error, notfound};
        {error, Reason} ->
            {error, {unknown, Reason}}
    end.

-spec decode_document(binary()) -> {ok, document()} | {error, notfound}.
decode_document(Response) ->
    case jsx:decode(Response) of
        #{<<"result">> := Result} ->
            {ok, Result};
        #{} ->
            {error, notfound}
    end.

-spec get_opa_client_opts() -> client_opts().
get_opa_client_opts() ->
    maps:merge(
        ?DEFAULT_CLIENT_OPTS,
        application:get_env(bouncer, opa, #{})
    ).

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
