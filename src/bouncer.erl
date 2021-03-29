-module(bouncer).

%% Application callbacks

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% Supervisor callbacks

-behaviour(supervisor).

-export([init/1]).

%%

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {AuditSpecs, AuditPulse} = get_audit_specs(),
    ServiceOpts = genlib_app:env(?MODULE, services, #{}),
    EventHandlers = genlib_app:env(?MODULE, woody_event_handlers, [woody_event_handler_default]),
    Healthcheck = enable_health_logging(genlib_app:env(?MODULE, health_check, #{})),
    {OpaClient, OpaClientSpec} = bouncer_opa_client:init(get_opa_opts()),
    WoodySpec = woody_server:child_spec(
        ?MODULE,
        #{
            ip => get_ip_address(),
            port => get_port(),
            protocol_opts => get_protocol_opts(),
            transport_opts => get_transport_opts(),
            shutdown_timeout => get_shutdown_timeout(),
            event_handler => EventHandlers,
            handlers =>
                get_handler_specs(ServiceOpts, AuditPulse, OpaClient) ++
                get_stub_handler_specs(ServiceOpts),
            additional_routes => [erl_health_handle:get_route(Healthcheck)]
        }
    ),
    {ok, {
        #{strategy => one_for_one, intensity => 10, period => 10},
        AuditSpecs ++ [OpaClientSpec, WoodySpec]
    }}.

-spec get_audit_specs() -> {[supervisor:child_spec()], bouncer_arbiter_pulse:handlers()}.
get_audit_specs() ->
    Opts = genlib_app:env(?MODULE, audit, #{}),
    case maps:get(log, Opts, #{}) of
        LogOpts = #{} ->
            {ok, ChildSpec, Pulse} = bouncer_audit_log:child_spec(LogOpts),
            {[ChildSpec], [Pulse]};
        disable ->
            {[], []}
    end.

-spec get_ip_address() -> inet:ip_address().
get_ip_address() ->
    {ok, Address} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    Address.

-spec get_port() -> inet:port_number().
get_port() ->
    genlib_app:env(?MODULE, port, 8022).

-spec get_protocol_opts() -> woody_server_thrift_http_handler:protocol_opts().
get_protocol_opts() ->
    genlib_app:env(?MODULE, protocol_opts, #{}).

-spec get_transport_opts() -> woody_server_thrift_http_handler:transport_opts().
get_transport_opts() ->
    genlib_app:env(?MODULE, transport_opts, #{}).

-spec get_shutdown_timeout() -> timeout().
get_shutdown_timeout() ->
    genlib_app:env(?MODULE, shutdown_timeout, 0).

-spec get_handler_specs(map(), bouncer_arbiter_pulse:handlers(), bouncer_opa_client:client()) ->
    [woody:http_handler(woody:th_handler())].
get_handler_specs(ServiceOpts, AuditPulse, OpaClient) ->
    ArbiterService = maps:get(arbiter, ServiceOpts, #{}),
    ArbiterPulse = maps:get(pulse, ArbiterService, []),
    ArbiterOpts = #{
        pulse => AuditPulse ++ ArbiterPulse,
        opa_client => OpaClient
    },
    [
        {
            maps:get(path, ArbiterService, <<"/v1/arbiter">>),
            {{bouncer_decisions_thrift, 'Arbiter'}, {bouncer_arbiter_handler, ArbiterOpts}}
        }
    ].

%% TODO delete after org_management is done
get_stub_handler_specs(ServiceOpts) ->
    OrgManagementStub = maps:get(org_management, ServiceOpts, #{}),
    [
        {
            maps:get(path, OrgManagementStub, <<"/v1/org_management_stub">>),
            {{orgmgmt_auth_context_provider_thrift, 'AuthContextProvider'},
                bouncer_org_management_stub}
        }
    ].

-spec get_opa_opts() -> bouncer_opa_client:opts().
get_opa_opts() ->
    genlib_app:env(?MODULE, opa, #{}).

%%

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(
        fun(_, Runner) -> #{runner => Runner, event_handler => EvHandler} end,
        Check
    ).
