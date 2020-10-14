-module(bouncer_context_v1).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-type vsn() :: integer().
-type format() :: thrift.

-type metadata() :: #{
    version := #{
        current  := vsn(),
        original := vsn(),
        latest   := vsn()
    }
}.

-export([decode/2]).
-export([encode/2]).

%%

-define(THRIFT_TYPE,
    {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}}).

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

-spec from_thrift(thrift_ctx_fragment()) ->
    {ok, bouncer_context:ctx(), metadata()}.
from_thrift(#bctx_v1_ContextFragment{} = Ctx0) ->
    Ctx1 = try_upgrade(Ctx0),
    Metadata = #{
        version => #{
            current  => Ctx1#bctx_v1_ContextFragment.vsn,
            original => Ctx0#bctx_v1_ContextFragment.vsn,
            latest   => ?BCTX_V1_HEAD
        }
    },
    {ok, from_thrift_context(Ctx1), Metadata}.

from_thrift_context(Ctx) ->
    {struct, _, [_VsnField | StructDef]} =
        bouncer_context_v1_thrift:struct_info('ContextFragment'),
    from_thrift_struct(StructDef, Ctx, 3, #{}).

from_thrift_struct(StructDef, Struct) ->
    from_thrift_struct(StructDef, Struct, 2, #{}).

from_thrift_struct([{_, _Req, Type, Name, _Default} | Rest], Struct, Idx, Acc) ->
    Acc1 = case element(Idx, Struct) of
        V when V /= undefined ->
            Acc#{Name => from_thrift_value(Type, V)};
        undefined ->
            Acc
    end,
    from_thrift_struct(Rest, Struct, Idx + 1, Acc1);
from_thrift_struct([], _Struct, _, Acc) ->
    Acc.

from_thrift_value({struct, struct, {Mod, Name}}, V) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    from_thrift_struct(StructDef, V);
from_thrift_value({set, Type}, Vs) ->
    ordsets:from_list([from_thrift_value(Type, V) || V <- ordsets:to_list(Vs)]);
from_thrift_value(string, V) ->
    V;
from_thrift_value(i64, V) ->
    V;
from_thrift_value(i32, V) ->
    V;
from_thrift_value(i16, V) ->
    V;
from_thrift_value(byte, V) ->
    V.

-spec try_upgrade(thrift_ctx_fragment()) ->
    thrift_ctx_fragment().
try_upgrade(#bctx_v1_ContextFragment{vsn = ?BCTX_V1_HEAD} = Ctx) ->
    Ctx.

%%

-spec encode(format(), bouncer_context:ctx()) ->
    {ok, _Content} | {error, _}.
encode(thrift, Context) ->
    Codec = thrift_strict_binary_codec:new(),
    try to_thrift(Context) of
        CtxThrift ->
            case thrift_strict_binary_codec:write(Codec, ?THRIFT_TYPE, CtxThrift) of
                {ok, Codec1} ->
                    {ok, thrift_strict_binary_codec:close(Codec1)};
                {error, _} = Error ->
                    Error
            end
    catch throw:{?MODULE, Reason} ->
        {error, Reason}
    end.

-spec to_thrift(bouncer_context:ctx()) ->
    thrift_ctx_fragment() | no_return().
to_thrift(Context) ->
    {struct, _, StructDef} = bouncer_context_v1_thrift:struct_info('ContextFragment'),
    to_thrift_struct(StructDef, Context, #bctx_v1_ContextFragment{}).

to_thrift_struct([{Idx, _Req, Type, Name, Default} | Rest], Map, Acc) ->
    case maps:take(Name, Map) of
        {V, MapLeft} ->
            Acc1 = erlang:setelement(Idx + 1, Acc, to_thrift_value(Type, V)),
            to_thrift_struct(Rest, MapLeft, Acc1);
        error when Default /= undefined ->
            Acc1 = erlang:setelement(Idx + 1, Acc, Default),
            to_thrift_struct(Rest, Map, Acc1);
        error ->
            to_thrift_struct(Rest, Map, Acc)
    end;
to_thrift_struct([], MapLeft, Acc) ->
    case map_size(MapLeft) of
        0 ->
            Acc;
        _ ->
            throw({?MODULE, {excess_context_data, MapLeft}})
    end.

to_thrift_value({struct, struct, {Mod, Name}}, V = #{}) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    Acc = erlang:make_tuple(length(StructDef) + 1, undefined, [{1, Mod:record_name(Name)}]),
    to_thrift_struct(StructDef, V, Acc);
to_thrift_value({set, Type}, Vs) ->
    ordsets:from_list([to_thrift_value(Type, V) || V <- ordsets:to_list(Vs)]);
to_thrift_value(string, V) ->
    V;
to_thrift_value(i64, V) ->
    V;
to_thrift_value(i32, V) ->
    V;
to_thrift_value(i16, V) ->
    V;
to_thrift_value(byte, V) ->
    V.
