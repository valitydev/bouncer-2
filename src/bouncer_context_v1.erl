-module(bouncer_context_v1).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-type vsn() :: integer().
-type format() :: thrift.

-type metadata() :: #{
    version := #{
        current := vsn(),
        original := vsn(),
        latest := vsn()
    }
}.

-export([decode/2]).
-export([encode/2]).

%%

-define(THRIFT_TYPE,
    {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}}
).

-type thrift_ctx_fragment() :: bouncer_context_v1_thrift:'ContextFragment'().

-spec decode(format(), _Content :: binary()) ->
    {ok, bouncer_context:ctx(), metadata()} | {error, _Reason}.
decode(thrift, Content) ->
    Codec = thrift_strict_binary_codec:new(Content),
    case thrift_strict_binary_codec:read(Codec, ?THRIFT_TYPE) of
        {ok, CtxThrift, Codec1} ->
            case thrift_strict_binary_codec:close(Codec1) of
                <<>> ->
                    from_thrift(CtxThrift);
                Leftovers ->
                    {error, {excess_binary_data, Leftovers}}
            end;
        Error ->
            Error
    end.

-spec from_thrift(thrift_ctx_fragment()) -> {ok, bouncer_context:ctx(), metadata()}.
from_thrift(#bctx_v1_ContextFragment{} = Ctx0) ->
    Ctx1 = try_upgrade(Ctx0),
    Metadata = #{
        version => #{
            current => Ctx1#bctx_v1_ContextFragment.vsn,
            original => Ctx0#bctx_v1_ContextFragment.vsn,
            latest => ?BCTX_V1_HEAD
        }
    },
    {ok, from_thrift_context(Ctx1), Metadata}.

from_thrift_context(Ctx) ->
    {struct, _, [_VsnField | StructDef]} =
        bouncer_context_v1_thrift:struct_info('ContextFragment'),
    % NOTE
    % This 3 refers to the first data field in a ContextFragment, after version field.
    bouncer_thrift:from_thrift_struct(StructDef, Ctx, 3, #{}).

-spec try_upgrade(thrift_ctx_fragment()) -> thrift_ctx_fragment().
try_upgrade(#bctx_v1_ContextFragment{vsn = 1} = Ctx) ->
    % no legacy data producers
    % legacy structures have been removed
    % nothing to update
    Ctx#bctx_v1_ContextFragment{vsn = ?BCTX_V1_HEAD};
try_upgrade(#bctx_v1_ContextFragment{vsn = ?BCTX_V1_HEAD} = Ctx) ->
    Ctx.

%%

-spec encode(format(), bouncer_context:ctx()) -> {ok, _Content} | {error, _}.
encode(thrift, Context) ->
    Codec = thrift_strict_binary_codec:new(),
    CtxThrift = to_thrift(Context),
    case thrift_strict_binary_codec:write(Codec, ?THRIFT_TYPE, CtxThrift) of
        {ok, Codec1} ->
            {ok, thrift_strict_binary_codec:close(Codec1)};
        {error, _} = Error ->
            Error
    end.

-spec to_thrift(bouncer_context:ctx()) -> thrift_ctx_fragment() | no_return().
to_thrift(Context) ->
    {struct, _, StructDef} = bouncer_context_v1_thrift:struct_info('ContextFragment'),
    bouncer_thrift:to_thrift_struct(StructDef, Context, #bctx_v1_ContextFragment{}).
