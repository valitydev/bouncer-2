%% TODO delete after org_management is done
-module(bouncer_stub_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("org_management_proto/include/orgmgmt_context_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([orgmgmt_get_user_context_ok/1]).

-type config() :: ct_helper:config().
-type group_name() :: atom().
-type test_case_name() :: atom().

-define(CONFIG(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

%%

-define(OPA_HOST, "opa").
-define(OPA_ENDPOINT, {?OPA_HOST, 8181}).
-define(API_RULESET_ID, "service/authz/api").

-spec all() -> [atom()].
all() ->
    [
        {group, general}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {general, [parallel], [
            orgmgmt_get_user_context_ok
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps =
        genlib_app:start_application(woody) ++
            genlib_app:start_application_with(scoper, [
                {storage, scoper_storage_logger}
            ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) -> ok.
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?CONFIG(suite_apps, C)).

-spec init_per_group(group_name(), config()) -> config().
init_per_group(Name, C) when Name == general ->
    start_bouncer([], C);
init_per_group(_Name, C) ->
    C.

start_bouncer(Env, C) ->
    IP = "127.0.0.1",
    Port = 8022,
    OrgmgmtPath = <<"/v1/org_management_stub">>,
    Apps = genlib_app:start_application_with(
        bouncer,
        [
            {ip, IP},
            {port, Port},
            {services, #{
                org_management => #{
                    path => OrgmgmtPath
                }
            }},
            {opa, #{
                endpoint => ?OPA_ENDPOINT,
                pool_opts => #{
                    connection_opts => #{
                        transport => tcp
                    }
                }
            }}
        ] ++ Env
    ),
    Services = #{
        org_management => mk_url(IP, Port, OrgmgmtPath)
    },
    [{group_apps, Apps}, {service_urls, Services} | C].

mk_url(IP, Port, Path) ->
    iolist_to_binary(["http://", IP, ":", genlib:to_binary(Port), Path]).

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Name, C) ->
    stop_bouncer(C).

stop_bouncer(C) ->
    ct_helper:with_config(
        group_apps,
        C,
        fun(Apps) -> genlib_app:stop_unload_applications(Apps) end
    ).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(Name, C) ->
    [{testcase, Name} | C].

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_Name, _C) ->
    ok.

%%

-spec orgmgmt_get_user_context_ok(config()) -> _.
orgmgmt_get_user_context_ok(C) ->
    Client = mk_client(C),
    UserID = <<"UserID">>,
    ?assertMatch(
        #bctx_ContextFragment{
            type = v1_thrift_binary,
            content = _Content
        },
        call_orgmgmt(UserID, Client)
    ).

mk_client(C) ->
    WoodyCtx = woody_context:new(genlib:to_binary(?CONFIG(testcase, C))),
    ServiceURLs = ?CONFIG(service_urls, C),
    {WoodyCtx, ServiceURLs}.

call_orgmgmt(UserID, Client) ->
    call(org_management, 'GetUserContext', {UserID}, Client).

call(ServiceName, Fn, Args, {WoodyCtx, ServiceURLs}) ->
    Service = get_service_spec(ServiceName),
    Opts = #{
        url => maps:get(ServiceName, ServiceURLs),
        event_handler => scoper_woody_event_handler
    },
    case woody_client:call({Service, Fn, Args}, Opts, WoodyCtx) of
        {ok, Response} ->
            Response;
        {exception, Exception} ->
            throw(Exception)
    end.

get_service_spec(org_management) ->
    {orgmgmt_auth_context_provider_thrift, 'AuthContextProvider'}.
