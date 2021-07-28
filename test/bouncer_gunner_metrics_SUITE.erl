-module(bouncer_gunner_metrics_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-type config() :: ct_helper:config().
-type group_name() :: atom().
-type test_case_name() :: atom().

-export([basic_metrics_test/1]).

%%

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").

-define(CONFIG(Key, C), (element(2, lists:keyfind(Key, 1, C)))).
-define(OPA_HOST, "opa").
-define(OPA_ENDPOINT, {?OPA_HOST, 8181}).

-spec all() -> [atom()].
all() ->
    [
        basic_metrics_test
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [].

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
init_per_group(Name, C) ->
    [{groupname, Name} | C].

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Name, _C) ->
    ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(Name, C) ->
    start_bouncer([], [{testcase, Name} | C]).

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_Name, C) ->
    stop_bouncer(C).

%%

-spec basic_metrics_test(config()) -> _.
basic_metrics_test(C) ->
    _ = call_judge("service/authz/api", #bdcs_Context{fragments = #{}}, mk_client(C)),
    _ = timer:sleep(100),
    ?assertEqual(25, get_metric([gunner, config, connections, max])),
    ?assertEqual(5, get_metric([gunner, config, connections, min])),
    ?assertEqual(1, get_metric([gunner, acquire, started])),
    ?assertEqual(1, get_metric([gunner, connection, init, started])),
    ?assertEqual(1, get_metric([gunner, connection, init, finished, ok])),
    ?assertEqual(1, get_metric([gunner, acquire, finished, ok])),
    ConnectionInits = get_metric([gunner, connection, init, finished, ok]),
    ConnectionDowns = get_metric([gunner, connection, down, normal]),
    ?assertEqual(1, ConnectionInits - ConnectionDowns).

%%

get_metric(Key) ->
    case ct_hay_publisher:get_metric(Key) of
        undefined ->
            0;
        Metric ->
            Metric
    end.

mk_client(C) ->
    WoodyCtx = woody_context:new(genlib:to_binary(?CONFIG(testcase, C))),
    ServiceURLs = ?CONFIG(service_urls, C),
    {WoodyCtx, ServiceURLs}.

call_judge(RulesetID, Context, Client) ->
    call(arbiter, 'Judge', {genlib:to_binary(RulesetID), Context}, Client).

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

get_service_spec(arbiter) ->
    {bouncer_decisions_thrift, 'Arbiter'}.

%%

start_bouncer(Env, C) ->
    IP = "127.0.0.1",
    Port = 8022,
    ArbiterPath = <<"/v1/arbiter">>,
    Apps0 = genlib_app:start_application_with(
        how_are_you,
        [
            {metrics_publishers, [ct_hay_publisher]},
            {metrics_handlers, []}
        ]
    ),
    Apps1 = genlib_app:start_application_with(
        bouncer,
        [
            {ip, IP},
            {port, Port},
            {services, #{
                arbiter => #{path => ArbiterPath}
            }},
            {transport_opts, #{
                max_connections => 1000,
                num_acceptors => 4
            }},
            {opa, #{
                endpoint => ?OPA_ENDPOINT,
                pool_opts => #{
                    event_handler => {bouncer_gunner_metrics_event_h, #{}},
                    connection_opts => #{
                        transport => tcp
                    }
                }
            }}
        ] ++ Env
    ),
    Services = #{
        arbiter => mk_url(IP, Port, ArbiterPath)
    },
    [{testcase_apps, Apps0 ++ Apps1}, {service_urls, Services} | C].

mk_url(IP, Port, Path) ->
    iolist_to_binary(["http://", IP, ":", genlib:to_binary(Port), Path]).

stop_bouncer(C) ->
    ct_helper:with_config(
        testcase_apps,
        C,
        fun(Apps) -> genlib_app:stop_unload_applications(Apps) end
    ).
