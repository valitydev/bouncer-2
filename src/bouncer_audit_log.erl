-module(bouncer_audit_log).

-export([init/1]).
-export([stop/1]).

-behaviour(bouncer_arbiter_pulse).

-export([handle_beat/3]).

-define(DEFAULT_LOG_LEVEL, notice).
-define(DEFAULT_FLUSH_QLEN, 10000).
-define(LOG_DOMAIN, [audit]).

-type opts() :: #{
    log => log_opts() | disabled
}.

% NOTE
% Keep in sync with `opts()`.
-define(OPTS, [log]).

-type log_opts() :: #{
    % Which log level to use for audit events? Defaults to `notice`.
    level => logger:level(),
    backend => logger_backend_opts(),
    % http://erlang.org/doc/man/logger.html#type-formatter_config
    formatter => {module(), logger:formatter_config()}
}.

% NOTE
% Keep in sync with `log_opts()`.
-define(LOG_OPTS, [level, backend, formatter]).

-type logger_backend_opts() :: #{
    % Where to log? Defaults to `standard_io`.
    type => standard_io | standard_error | file,
    % Log file location. No default, MUST be set if `type` is `file`.
    file => file:filename(),
    % http://erlang.org/doc/man/logger_std_h.html
    max_no_bytes => pos_integer() | infinity,
    max_no_files => non_neg_integer(),
    % Maximum number of events to queue for writing. Defaults to 10000.
    % http://erlang.org/doc/apps/kernel/logger_chapter.html#message-queue-length
    flush_qlen => non_neg_integer()
}.

% NOTE
% Keep in sync with `logger_backend_opts()`.
-define(LOGGER_BACKEND_OPTS, [type, file, max_no_bytes, max_no_files, flush_qlen]).

-export_type([opts/0]).

%%

-type st() ::
    {log, logger:level()}.

-spec init(opts()) -> bouncer_arbiter_pulse:handlers(st()).
init(Opts) ->
    _ = assert_strict_opts(?OPTS, Opts),
    init_log_handler(maps:get(log, Opts, #{})).

init_log_handler(LogOpts = #{}) ->
    _ = assert_strict_opts(?LOG_OPTS, LogOpts),
    Level = validate_log_level(maps:get(level, LogOpts, ?DEFAULT_LOG_LEVEL)),
    BackendConfig = mk_logger_backend_config(maps:get(backend, LogOpts, #{})),
    HandlerConfig0 = maps:with([formatter], LogOpts),
    HandlerConfig1 = HandlerConfig0#{
        config => BackendConfig,
        % NOTE
        % This two options together ensure that _only_ audit logs will flow through to the backend.
        filters => [{domain, {fun logger_filters:domain/2, {log, sub, ?LOG_DOMAIN}}}],
        filter_default => stop
    },
    ok = logger:add_handler(
        ?MODULE,
        logger_std_h,
        HandlerConfig1
    ),
    % TODO
    % Validate that global logger level doesn't suppress ours?
    ok = log(Level, "audit log started", #{}),
    [{?MODULE, {log, Level}}];
init_log_handler(disabled) ->
    [].

validate_log_level(Level) ->
    eq = logger:compare_levels(Level, Level),
    Level.

mk_logger_backend_config(BackendOpts) ->
    _ = assert_strict_opts(?LOGGER_BACKEND_OPTS, BackendOpts),
    Type = validate_log_type(maps:get(type, BackendOpts, standard_io)),
    mk_logger_backend_config(Type, BackendOpts).

validate_log_type(Type) when Type == standard_io; Type == standard_error; Type == file ->
    Type;
validate_log_type(Type) ->
    erlang:error(badarg, [Type]).

mk_logger_backend_config(file = Type, Opts) ->
    Defaults = get_default_backend_config(Type, Opts),
    Filename = maps:get(file, Opts),
    Config0 = maps:with([max_no_bytes, max_no_files], Opts),
    Config = maps:merge(Defaults, Config0),
    Config#{
        type => Type,
        file => Filename
    };
mk_logger_backend_config(Type, Opts) ->
    Defaults = get_default_backend_config(Type, Opts),
    Defaults#{
        type => Type
    }.

get_default_backend_config(file, Opts) ->
    % NOTE
    % All those options chosen to push message loss probability as close to zero as possible.
    % Zero doesn't seem reachable with standard logger infrastructure because of various safeguards
    % around unexpected backend and formatter errors.
    Config = get_default_backend_config(Opts),
    Config#{
        % Protects against accidental write loss upon file rotation.
        file_check => 0
    };
get_default_backend_config(_Type, Opts) ->
    get_default_backend_config(Opts).

get_default_backend_config(Opts) ->
    FlushQLen = maps:get(flush_qlen, Opts, ?DEFAULT_FLUSH_QLEN),
    #{
        % No need to set it up here since we'll sync on EVERY write by ourself.
        filesync_repeat_interval => no_repeat,

        % http://erlang.org/doc/apps/kernel/logger_chapter.html#message-queue-length
        sync_mode_qlen => 0,
        drop_mode_qlen => FlushQLen,
        flush_qlen => FlushQLen,

        % http://erlang.org/doc/apps/kernel/logger_chapter.html#controlling-bursts-of-log-requests
        burst_limit_enable => false,

        % http://erlang.org/doc/apps/kernel/logger_chapter.html#terminating-an-overloaded-handler
        overload_kill_enable => false
    }.

assert_strict_opts(Ks, Opts) ->
    case maps:without(Ks, Opts) of
        Empty when map_size(Empty) == 0 ->
            ok;
        Unrecognized ->
            erlang:error({unrecognized_opts, Unrecognized})
    end.

%%

-spec stop(opts()) -> ok.
stop(Opts = #{}) ->
    stop_log_handler(maps:get(log, Opts, #{})).

-spec stop_log_handler(log_opts()) -> ok.
stop_log_handler(LogOpts = #{}) ->
    Level = maps:get(level, LogOpts, ?DEFAULT_LOG_LEVEL),
    ok = log(Level, "audit log stopped", #{}),
    _ = logger:remove_handler(?MODULE),
    ok;
stop_log_handler(disabled) ->
    ok.

%%

-type beat() :: bouncer_arbiter_pulse:beat().
-type metadata() :: bouncer_arbiter_pulse:metadata().

-spec handle_beat(beat(), metadata(), st()) -> ok.
handle_beat(Beat, Metadata, {log, Level}) ->
    log(
        get_severity(Beat, Level),
        get_message(Beat),
        extract_metadata(Metadata, get_beat_metadata(Beat))
    ).

log(Severity, Message, Metadata) ->
    DefaultMetadata = #{
        type => audit,
        domain => ?LOG_DOMAIN
    },
    % NOTE
    % Matching on `ok` here is crucial. Logger may decide to flush the queue behind the scenes so
    % we need to ensure it's not happening.
    ok = logger:log(Severity, Message, maps:merge(Metadata, DefaultMetadata)),
    ok = logger_std_h:filesync(?MODULE),
    ok.

get_severity({judgement, started}, _Level) -> debug;
get_severity(_, Level) -> Level.

get_message({judgement, started}) -> <<"judgement started">>;
get_message({judgement, {completed, _}}) -> <<"judgement completed">>;
get_message({judgement, {failed, _}}) -> <<"judgement failed">>.

get_beat_metadata({judgement, Event}) ->
    #{
        judgement =>
            case Event of
                started ->
                    #{
                        event => started
                    };
                {completed, {Resolution, Assertions}} ->
                    encode_restrictions(Resolution, #{
                        event => completed,
                        resolution => encode_resolution(Resolution),
                        assertions => lists:map(fun encode_assertion/1, Assertions)
                    });
                {failed, Error} ->
                    #{
                        event => failed,
                        error => encode_error(Error)
                    }
            end
    }.

encode_resolution(allowed) -> <<"allowed">>;
encode_resolution(forbidden) -> <<"forbidden">>;
encode_resolution({restricted, _Restrictions}) -> <<"restricted">>.

%% NOTE
%% I judged adding restrictions parsing to be not worth it for audit log
encode_restrictions({restricted, Restrictions}, Acc) ->
    Acc#{restrictions => Restrictions};
encode_restrictions(_, Acc) ->
    Acc.

encode_assertion({Code, Details}) ->
    #{code => Code, details => Details}.

encode_error({Class, Details}) when is_atom(Class) ->
    #{class => Class, details => genlib:format(Details)};
encode_error(Class) when is_atom(Class) ->
    #{class => Class};
encode_error(Other) ->
    #{details => genlib:format(Other)}.

extract_metadata(Metadata, Acc) ->
    Acc1 = extract_opt_meta(ruleset, Metadata, fun encode_id/1, Acc),
    Acc2 = extract_opt_meta(context, Metadata, fun encode_context/1, Acc1),
    Acc3 = extract_opt_meta(fragments, Metadata, fun encode_fragments/1, Acc2),
    extract_woody_ctx(maps:get(woody_ctx, Metadata, undefined), Acc3).

extract_opt_meta(K, Metadata, EncodeFun, Acc) ->
    case maps:find(K, Metadata) of
        {ok, V} -> Acc#{K => EncodeFun(V)};
        error -> Acc
    end.

encode_id(ID) when is_binary(ID) ->
    ID.

encode_context(Context = #{}) ->
    % TODO
    % We'll probably need something like `bouncer_context:extract_metadata/1` here when the schema
    % stops being that simple.
    Context.

encode_fragments(Fragments = #{}) ->
    maps:map(fun encode_fragment/2, Fragments).

encode_fragment(_ID, Fragment) ->
    maps:with([type, metadata], Fragment).

extract_woody_ctx(WoodyCtx = #{rpc_id := RpcID}, Acc) ->
    extract_woody_meta(WoodyCtx, extract_woody_rpc_id(RpcID, Acc));
extract_woody_ctx(undefined, Acc) ->
    Acc.

extract_woody_rpc_id(RpcID = #{span_id := _, trace_id := _, parent_id := _}, Acc) ->
    maps:merge(Acc, RpcID).

%% TODO
%% This currently duplicates scoper-related metadata in every audit log
%% message. However, scoper is not a must, so we should probably allow
%% to turn this thing on and off through options.
%%
%% extract_woody_meta(#{meta := Meta}, Acc) when map_size(Meta) > 0 ->
%%     Acc#{woody => #{metadata => Meta}};
%%     Acc;
extract_woody_meta(#{}, Acc) ->
    Acc.
