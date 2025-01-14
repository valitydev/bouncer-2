-module(bouncer_arbiter_handler).

-include_lib("bouncer_proto/include/bouncer_decision_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_rstn_thrift.hrl").

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-type opts() :: #{
    pulse => bouncer_arbiter_pulse:handlers(),
    opa_client := bouncer_opa_client:client()
}.

-record(st, {
    pulse :: bouncer_arbiter_pulse:handlers(),
    pulse_metadata :: bouncer_arbiter_pulse:metadata(),
    opa_client :: bouncer_opa_client:client()
}).

-type st() :: #st{}.

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), opts()) ->
    {ok, woody:result()}.
handle_function(Fn, Args, WoodyCtx, Opts) ->
    do_handle_function(Fn, Args, WoodyCtx, Opts).

do_handle_function('Judge', {RulesetID, ContextIn}, WoodyCtx, Opts) ->
    St = #st{
        pulse = maps:get(pulse, Opts, []),
        pulse_metadata = #{woody_ctx => WoodyCtx},
        opa_client = maps:get(opa_client, Opts)
    },
    try
        handle_judge(RulesetID, ContextIn, St)
    catch
        throw:{woody, Class, Details} ->
            woody_error:raise(Class, Details);
        C:R:S ->
            ok = handle_judgement_beat({failed, {unexpected_exception, {C, R, S}}}, St),
            erlang:raise(C, R, S)
    end.

%%

handle_judge(RulesetID, ContextIn, St0) ->
    St1 = append_pulse_metadata(#{ruleset => RulesetID}, St0),
    ok = handle_judgement_beat(started, St1),
    {Context, St2} = decode_context(ContextIn, St1),
    case bouncer_arbiter:judge(RulesetID, Context, St2#st.opa_client) of
        {ok, Judgement} ->
            ok = handle_judgement_beat({completed, Judgement}, St2),
            {ok, encode_judgement(Judgement)};
        {error, ruleset_notfound = Reason} ->
            ok = handle_judgement_beat({failed, Reason}, St2),
            throw({woody, business, #decision_RulesetNotFound{}});
        {error, {ruleset_invalid, _} = Reason} ->
            ok = handle_judgement_beat({failed, Reason}, St2),
            throw({woody, business, #decision_InvalidRuleset{}});
        {error, Reason} ->
            handle_network_error(Reason, St2)
    end.

-spec handle_network_error(_Reason, st()) -> no_return().
handle_network_error({unavailable, Reason} = Error, St) ->
    ok = handle_judgement_beat({failed, Error}, St),
    throw({woody, system, {external, resource_unavailable, genlib:format(Reason)}});
handle_network_error({unknown, Reason} = Error, St) ->
    ok = handle_judgement_beat({failed, Error}, St),
    throw({woody, system, {external, result_unknown, genlib:format(Reason)}}).

%%

-type fragment_id() :: binary().
-type fragment_metadata() :: #{atom() => _}.

-type thrift_judgement() :: bouncer_decision_thrift:'Judgement'().
-type thrift_context() :: bouncer_decision_thrift:'Context'().
-type thrift_fragment() :: bouncer_ctx_thrift:'ContextFragment'().
-type thrift_fragment_type() :: bouncer_ctx_thrift:'ContextFragmentType'().

-spec encode_judgement(bouncer_arbiter:judgement()) -> thrift_judgement().
encode_judgement({Resolution, _Assertions}) ->
    #decision_Judgement{
        resolution = encode_resolution(Resolution)
    }.

encode_resolution(allowed) ->
    {allowed, #decision_ResolutionAllowed{}};
encode_resolution(forbidden) ->
    {forbidden, #decision_ResolutionForbidden{}};
encode_resolution({restricted, Restrictions}) ->
    {restricted, #decision_ResolutionRestricted{
        restrictions = encode_restrictions(Restrictions)
    }}.

encode_restrictions(Restrictions) ->
    {struct, _, StructDef} = bouncer_rstn_thrift:struct_info('Restrictions'),
    bouncer_thrift:json_to_thrift_struct(StructDef, Restrictions, #rstn_Restrictions{}).

-spec decode_context(thrift_context(), st()) -> {bouncer_context:ctx(), st()}.
decode_context(#decision_Context{fragments = FragmentsIn}, St0) ->
    % 1. Decode each fragment.
    {Fragments, St1} = decode_fragments(FragmentsIn, St0),
    % 2. Merge each decoded context into an empty context. Accumulate conflicts associated with
    % corresponding fragment id.
    {Ctx, Conflicts} = maps:fold(
        fun(ID, Ctx, {CtxAcc, DiscardAcc}) ->
            case bouncer_context:merge(CtxAcc, Ctx) of
                {CtxAcc1, undefined} ->
                    {CtxAcc1, DiscardAcc};
                {CtxAcc1, Discard} ->
                    {CtxAcc1, #{ID => Discard}}
            end
        end,
        {bouncer_context:empty(), #{}},
        Fragments
    ),
    % 3. Return merged context if there was no conflicts.
    case map_size(Conflicts) of
        0 ->
            St2 = append_pulse_metadata(#{context => Ctx}, St1),
            {Ctx, St2};
        _ ->
            % TODO
            % Вообще можно на сторону политик отдать ответственность за проверку отстутствия
            % конфликтов. Так как конфликты возможны (подозреваю, что в процессе эволюции
            % системы рано или поздно они где-нибудь появятся), быть может стоит это сделать
            % сразу?
            ok = handle_judgement_beat({failed, {conflicting_context, Conflicts}}, St1),
            throw({woody, business, #decision_InvalidContext{}})
    end.

-spec decode_fragments(#{fragment_id() => thrift_fragment()}, st()) ->
    {#{fragment_id() => bouncer_context:ctx()}, st()}.
decode_fragments(Fragments, St0) ->
    {Ctxs, Errors, PulseMeta} = maps:fold(
        fun(ID, Fragment, {CtxAcc, ErrorAcc, PulseMetaAcc}) ->
            Type = Fragment#ctx_ContextFragment.type,
            Content = genlib:define(Fragment#ctx_ContextFragment.content, <<>>),
            case decode_fragment(Type, Content) of
                {ok, Ctx, Meta} ->
                    PulseMeta = #{
                        type => Type,
                        context => Ctx,
                        metadata => Meta
                    },
                    {
                        CtxAcc#{ID => Ctx},
                        ErrorAcc,
                        PulseMetaAcc#{ID => PulseMeta}
                    };
                {error, Reason} ->
                    {
                        CtxAcc,
                        ErrorAcc#{ID => {Type, Reason}},
                        PulseMetaAcc
                    }
            end
        end,
        {#{}, #{}, #{}},
        Fragments
    ),
    St1 = append_pulse_metadata(#{fragments => PulseMeta}, St0),
    case map_size(Errors) of
        0 ->
            {Ctxs, St1};
        _ ->
            ok = handle_judgement_beat({failed, {malformed_context, Errors}}, St1),
            throw({woody, business, #decision_InvalidContext{}})
    end.

-spec decode_fragment(thrift_fragment_type(), _Content :: binary()) ->
    {ok, bouncer_context:ctx(), fragment_metadata()} | {error, _Reason}.
decode_fragment(v1_thrift_binary, Content) ->
    bouncer_context_v1:decode(thrift, Content).

%%

-spec append_pulse_metadata(bouncer_arbiter_pulse:metadata(), st()) -> st().
append_pulse_metadata(Metadata, #st{pulse_metadata = MetadataWas} = St) ->
    St#st{pulse_metadata = maps:merge(MetadataWas, Metadata)}.

-spec handle_judgement_beat(_Beat, st()) -> ok.
handle_judgement_beat(Beat, #st{pulse = Pulse, pulse_metadata = Metadata}) ->
    bouncer_arbiter_pulse:handle_beat({judgement, Beat}, Metadata, Pulse).
