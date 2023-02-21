-module(bouncer_opa_client_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([request_document_not_found/1]).

-type config() :: ct_helper:config().

%%

-define(OPA_HOST, "opa").
-define(OPA_ENDPOINT_RESOLVE, {{resolve, dns, ?OPA_HOST, #{pick => random}}, 8181}).
-define(OPA_ENDPOINT, {?OPA_HOST, 8181}).

-spec all() -> list(atom()).
all() ->
    [
        request_document_not_found
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    C.

-spec end_per_suite(config()) -> ok.
end_per_suite(_C) ->
    ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_Name, C) ->
    C.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_Name, _C) ->
    ok.

%%

-spec request_document_not_found(config()) -> ok.
request_document_not_found(_C) ->
    OpaClient = setup_gunner_pool_and_opa_proxy_client(),
    Input = #{},
    Result = bouncer_opa_client:request_document(<<"non/existing/ruleset">>, Input, OpaClient),
    ?assertMatch({error, notfound}, Result).

%%

setup_gunner_pool_and_opa_proxy_client() ->
    {OpaClient, OpaClientSpec} = bouncer_opa_client:init(#{
        endpoint => ?OPA_ENDPOINT_RESOLVE,
        request_timeout => 1000,
        pool_opts => #{
            connection_opts => #{
                transport => tcp
            }
        }
    }),
    {gunner_pool, start_link, [PoolReg, PoolOpts]} = maps:get(start, OpaClientSpec),
    {ok, _} = gunner_pool:start_link(PoolReg, PoolOpts),
    {ok, _Proxy} = ct_proxy:start_link(?OPA_ENDPOINT, #{listen => ignore}),
    OpaClient.
