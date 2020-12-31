-module(bouncer_audit_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invalid_config_fails_start/1]).
-export([unrecognized_config_fails_start/1]).
-export([write_error_fails_request/1]).
-export([write_queue_overload_fails_request/1]).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").

-type config() :: ct_helper:config().
-type testcase_name() :: atom().

-define(CONFIG(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

%%

-define(OPA_HOST, "opa").
-define(OPA_ENDPOINT, {?OPA_HOST, 8181}).
-define(API_RULESET_ID, "service/authz/api").

-spec all() ->
    [atom()].

all() ->
    [
        invalid_config_fails_start,
        unrecognized_config_fails_start,
        write_error_fails_request
        % TODO
        % This testcase is currently failing consistently.
        % Turns out logger (as of Erlang/OTP 23 w/ kernel 7.0) doesn't tell callers when specific
        % handler loses an event due to queue flushing, even though doc says it should have.
        % Best bet is to max out `flush_qlen` option in the meantime.
        % write_queue_overload_fails_request
    ].

-spec init_per_suite(config()) ->
    config().

init_per_suite(C) ->
    Apps =
        genlib_app:start_application(woody) ++
        genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_logger}
        ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) ->
    ok.
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?CONFIG(suite_apps, C)).

-spec init_per_testcase(testcase_name(), config()) ->
    config().

init_per_testcase(Name, C) ->
    [{testcase, Name} | C].

-spec end_per_testcase(testcase_name(), config()) ->
    config().

end_per_testcase(_Name, _C) ->
    ok.

%%

-define(CONTEXT(Fragments), #bdcs_Context{fragments = Fragments}).

-spec invalid_config_fails_start(config()) -> ok.
-spec unrecognized_config_fails_start(config()) -> ok.
-spec write_error_fails_request(config()) -> ok.
-spec write_queue_overload_fails_request(config()) -> ok.

invalid_config_fails_start(C) ->
    ?assertError(
        {bouncer, {{{badkey, file}, _Stacktrace}, _}},
        start_stop_bouncer([
            {audit, #{log => #{
                backend => #{
                    % NOTE
                    % Missing target filename here.
                    type => file
                }
            }}}
        ], C)
    ),
    ?assertError(
        {bouncer, {{badarg, _Stacktrace}, _}},
        start_stop_bouncer([
            {audit, #{log => #{
                level => blarg
            }}}
        ], C)
    ).

unrecognized_config_fails_start(C) ->
    ?assertError(
        {bouncer, {{{unrecognized_opts, #{blarg := _}}, _Stacktrace}, _}},
        start_stop_bouncer([
            {audit, #{blarg => blorg}}
        ], C)
    ),
    ?assertError(
        {bouncer, {{{unrecognized_opts, #{blarg := _}}, _Stacktrace}, _}},
        start_stop_bouncer([
            {audit, #{log => #{
                blarg => blorg
            }}}
        ], C)
    ),
    ?assertError(
        {bouncer, {{{unrecognized_opts, #{hello := _}}, _Stacktrace}, _}},
        start_stop_bouncer([
            {audit, #{log => #{
                level => notice,
                hello => <<"mike">>
            }}}
        ], C)
    ).

start_stop_bouncer(Env, C) ->
    % NOTE
    % Just to be sure to have clean state inbetween testcases.
    stop_bouncer(start_bouncer(Env, C)).

write_error_fails_request(C) ->
    Dirname = mk_temp_dir(?CONFIG(testcase, C)),
    Filename = filename:join(Dirname, "audit.log"),
    C1 = start_bouncer([{audit, #{
        log => #{
            backend => #{type => file, file => Filename}
        }
    }}], C),
    Client = mk_client(C1),
    try
        ok = file:delete(Filename),
        ok = file:change_mode(Dirname, 8#555),
        ?assertError(
            {woody_error, {external, result_unexpected, _}},
            call_judge(?API_RULESET_ID, ?CONTEXT(#{}), Client)
        )
    after
        _ = rm_temp_dir(Dirname),
        stop_bouncer(C1)
    end.

write_queue_overload_fails_request(C) ->
    QLen = 10,
    Concurrency = QLen * 10,
    Dirname = mk_temp_dir(?CONFIG(testcase, C)),
    Filename = filename:join(Dirname, "audit.log"),
    C1 = start_bouncer([{audit, #{
        log => #{
            backend => #{type => file, file => Filename, flush_qlen => QLen},
            formatter => {logger_logstash_formatter, #{single_line => true}}
        }
    }}], C),
    Client = mk_client(C1),
    Results = genlib_pmap:safemap(
        fun (_) ->
            call_judge(?API_RULESET_ID, ?CONTEXT(#{}), Client)
        end,
        lists:seq(1, Concurrency)
    ),
    _ = stop_bouncer(C1),
    try
        {Succeeded, _Failed} = lists:partition(fun ({R, _}) -> R == ok end, Results),
        {ok, LogfileContents} = file:read_file(Filename),
        NumLogEvents = binary:matches(LogfileContents, <<"\"type\":\"audit\"">>), % TODO kinda hacky
        ?assertEqual(length(Succeeded), length(NumLogEvents))
    after
        rm_temp_dir(Dirname)
    end.

mk_temp_dir(Name) ->
    TempDir = ct_helper:get_temp_dir(),
    Random = binary_to_list(genlib:unique()),
    TargetDir = filename:join([TempDir, Name, Random]),
    ok = filelib:ensure_dir(filename:join(TargetDir, ".")),
    TargetDir.

rm_temp_dir(Dirname) ->
    Root = filename:dirname(Dirname),
    case file:del_dir_r(Root) of
        ok ->
            ok;
        {error, Reason} ->
            ct:pal("unable to cleanup ~p: ~p", [Root, Reason])
    end.

%%

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
    Apps = start_application_with(bouncer, [
        {ip, IP},
        {port, Port},
        {services, #{
            arbiter => #{path => ArbiterPath}
        }},
        {transport_opts, #{
            max_connections => 1000,
            num_acceptors   => 4
        }},
        {opa, #{
            endpoint  => ?OPA_ENDPOINT,
            transport => tcp
        }}
    ] ++ Env),
    Services = #{
        arbiter => mk_url(IP, Port, ArbiterPath)
    },
    [{testcase_apps, Apps}, {service_urls, Services} | C].

mk_url(IP, Port, Path) ->
    iolist_to_binary(["http://", IP, ":", genlib:to_binary(Port), Path]).

stop_bouncer(C) ->
    ct_helper:with_config(testcase_apps, C,
        fun (Apps) -> genlib_app:stop_unload_applications(Apps) end).

start_application_with(App, Env) ->
    _ = application:load(App),
    _ = [application:set_env(App, K, V) || {K, V} <- Env],
    case application:ensure_all_started(App, temporary) of
        {ok, Apps} ->
            Apps;
        {error, Reason} ->
            _ = application:unload(App),
            erlang:error(Reason)
    end.
