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
-type ruleset_id() :: bouncer_opa_client:ruleset_id().
-type document() :: bouncer_opa_client:document().

-type judgement() :: {resolution(), [assertion()]}.
-type resolution() :: allowed | forbidden | {restricted, map()}.
-type assertion() :: {_Code :: binary(), _Details :: #{binary() => _}}.

-export_type([judgement/0]).
-export_type([resolution/0]).

-export([judge/3]).

%%

-spec judge(ruleset_id(), bouncer_context:ctx(), bouncer_opa_client:client()) ->
    {ok, judgement()}
    | {error,
        ruleset_notfound
        | {ruleset_invalid, _Details}
        | {unavailable | unknown, _Reason}}.
judge(RulesetID, Context, Client) ->
    case bouncer_opa_client:request_document(RulesetID, Context, Client) of
        {ok, Document} ->
            infer_judgement(Document);
        {error, notfound} ->
            {error, ruleset_notfound};
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
