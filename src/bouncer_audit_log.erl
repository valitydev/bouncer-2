-module(bouncer_audit_log).

-export([init/1]).
-export([stop/0]).

-behaviour(bouncer_arbiter_pulse).
-export([handle_beat/3]).

-type opts() :: #{
    log => log_opts() | disabled
}.

-type log_opts() :: #{
    % Where to log? Defaults to `standard_io`.
    type => standard_io | standard_error | file,
    % Log file location. No default, MUST be set if `type` is `file`.
    file => file:filename(),
    % See: http://erlang.org/doc/man/logger_std_h.html
    max_no_bytes => pos_integer() | infinity,
    max_no_files => non_neg_integer()
}.

-define(LOG_OPTS_KEYS, [
    % NOTE
    % Keep in sync w/ `log_opts()`.
    type,
    file,
    max_no_bytes,
    max_no_files
]).

-export_type([opts/0]).

%%

-define(LOG_DOMAIN, [audit]).
-define(DEFAULT_SANE_QLEN, 10000). % TODO make configurable?

-spec init(opts()) ->
    bouncer_arbiter_pulse:handlers().
init(Opts) ->
    init_log_handler(maps:get(log, Opts, #{})).

init_log_handler(LogOpts = #{}) ->
    _ = assert_strict_opts(?LOG_OPTS_KEYS, LogOpts),
    Type = validate_log_type(maps:get(type, LogOpts, standard_io)),
    BackendConfig = mk_log_config(Type, LogOpts),
    HandlerConfig = #{
        config  => BackendConfig,
        % NOTE
        % This two options together ensure that _only_ audit logs will flow through to the backend.
        filters => [{domain, {fun logger_filters:domain/2, {log, sub, ?LOG_DOMAIN}}}],
        filter_default => stop,
        % FIXME
        formatter => {logger_logstash_formatter, #{}}
    },
    ok = logger:add_handler(
        ?MODULE,
        logger_std_h,
        HandlerConfig
    ),
    ok = log(alert, "audit log started", #{}),
    [{?MODULE, log}];
init_log_handler(disabled) ->
    [].

validate_log_type(Type) when
    Type == standard_io;
    Type == standard_error;
    Type == file
->
    Type;
validate_log_type(Type) ->
    erlang:error({invalid_log_type, Type}).

mk_log_config(file = Type, Opts) ->
    Defaults = get_default_log_config(Type),
    Filename = maps:get(file, Opts),
    Config0 = maps:with([max_no_bytes, max_no_files], Opts),
    Config = maps:merge(Defaults, Config0),
    Config#{
        type => Type,
        file => Filename
    };
mk_log_config(Type, _Opts) ->
    Defaults = get_default_log_config(Type),
    Defaults#{
        type => Type
    }.

get_default_log_config(file) ->
    % NOTE
    % All those options chosen to push message loss probability as close to zero as possible.
    % Zero doesn't seem reachable with standard logger infrastructure because of various safeguards
    % around unexpected backend and formatter errors.
    Config = get_default_log_config(),
    Config#{
        % Protects against accidental write loss upon file rotation.
        file_check => 0
    };
get_default_log_config(_Type) ->
    get_default_log_config().

get_default_log_config() ->
    #{
        % No need to set it up here since we'll sync on EVERY write by ourself.
        filesync_repeat_interval => no_repeat,

        % See: http://erlang.org/doc/apps/kernel/logger_chapter.html#message-queue-length
        sync_mode_qlen => 0,
        drop_mode_qlen => ?DEFAULT_SANE_QLEN,
        flush_qlen => ?DEFAULT_SANE_QLEN,

        % See: http://erlang.org/doc/apps/kernel/logger_chapter.html#controlling-bursts-of-log-requests
        burst_limit_enable => false,

        % See: http://erlang.org/doc/apps/kernel/logger_chapter.html#terminating-an-overloaded-handler
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

-spec stop() ->
    ok.
stop() ->
    ok = log(alert, "audit log stopped", #{}),
    _ = logger:remove_handler(?MODULE),
    ok.

%%

-type beat()     :: bouncer_arbiter_pulse:beat().
-type metadata() :: bouncer_arbiter_pulse:metadata().

-type pulse_opts() :: log.

-spec handle_beat(beat(), metadata(), pulse_opts()) ->
    ok.
handle_beat(Beat, Metadata, log) ->
    log(
        get_severity(Beat),
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

get_severity({judgement, started}) -> debug;
get_severity(_)                    -> alert.

get_message({judgement, started})        -> "judgement started";
get_message({judgement, {completed, _}}) -> "judgement completed";
get_message({judgement, {failed, _}})    -> "judgement failed".

get_beat_metadata({judgement, Event}) ->
    #{
        judgement => case Event of
            started ->
                #{
                    event => started
                };
            {completed, {Resolution, Assertions}} ->
                #{
                    event => completed,
                    resolution => encode_resolution(Resolution),
                    assertions => lists:map(fun encode_assertion/1, Assertions)
                };
            {failed, Error} ->
                #{
                    event => failed,
                    error => encode_error(Error)
                }
        end
    }.

encode_resolution(allowed)   -> <<"allowed">>;
encode_resolution(forbidden) -> <<"forbidden">>.

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
        error   -> Acc
    end.

encode_id(ID) when is_binary(ID) ->
    ID.

encode_context(Context = #{}) ->
    % TODO
    % We'll probably need something like `bouncer_context:extract_metadata/1` here when the schema
    % stops being that simple.
    Context.

encode_fragments(Fragments = #{}) ->
    % TODO
    % See above.
    Fragments.

extract_woody_ctx(WoodyCtx = #{rpc_id := RpcID}, Acc) ->
    extract_woody_meta(WoodyCtx, extract_woody_rpc_id(RpcID, Acc));
extract_woody_ctx(undefined, Acc) ->
    Acc.

extract_woody_rpc_id(RpcID = #{span_id := _, trace_id := _, parent_id := _}, Acc) ->
    maps:merge(Acc, RpcID).

extract_woody_meta(#{meta := Meta}, Acc) when map_size(Meta) > 0 ->
    Acc#{woody => #{metadata => Meta}};
extract_woody_meta(#{}, Acc) ->
    Acc.
