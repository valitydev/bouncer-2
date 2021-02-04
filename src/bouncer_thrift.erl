-module(bouncer_thrift).

%% API
-export([json_to_thrift_struct/3]).
-export([to_thrift_struct/3]).
-export([from_thrift_struct/4]).

-type struct_flavour() :: struct | exception | union.
-type field_num() :: pos_integer().
-type field_name() :: atom().
-type field_req() :: required | optional | undefined.

-type type_ref() :: {module(), atom()}.
-type field_type() ::
    bool
    | byte
    | i16
    | i32
    | i64
    | string
    | double
    | {enum, type_ref()}
    | {struct, struct_flavour(), type_ref()}
    | {list, field_type()}
    | {set, field_type()}
    | {map, field_type(), field_type()}.

-type struct_field_info() ::
    {field_num(), field_req(), field_type(), field_name(), any()}.

-spec json_to_thrift_struct([struct_field_info()], map(), tuple()) -> tuple().
json_to_thrift_struct(StructDef, Map, Acc) ->
    % NOTE
    % This 2 refers to the first field in a record tuple.
    to_thrift_struct(StructDef, Map, 2, Acc, fun genlib:to_binary/1).

-spec to_thrift_struct([struct_field_info()], map(), tuple()) -> tuple().
to_thrift_struct(StructDef, Map, Acc) ->
    % NOTE
    % This 2 refers to the first field in a record tuple.
    to_thrift_struct(StructDef, Map, 2, Acc, fun identity/1).

to_thrift_struct([{_Tag, _Req, Type, Name, Default} | Rest], Map, Idx, Acc, NameFun) ->
    TransformedName = NameFun(Name),
    case maps:take(TransformedName, Map) of
        {V, MapLeft} ->
            Acc1 = erlang:setelement(Idx, Acc, to_thrift_value(Type, V, NameFun)),
            to_thrift_struct(Rest, MapLeft, Idx + 1, Acc1, NameFun);
        error when Default /= undefined ->
            Acc1 = erlang:setelement(Idx, Acc, Default),
            to_thrift_struct(Rest, Map, Idx + 1, Acc1, NameFun);
        error ->
            to_thrift_struct(Rest, Map, Idx + 1, Acc, NameFun)
    end;
to_thrift_struct([], MapLeft, _Idx, Acc, _NameFun) ->
    case map_size(MapLeft) of
        0 ->
            Acc;
        _ ->
            %% Some part of map was left after converting to thrift,
            %% indicating that either thrift structure doesn't have
            %% enough fields or there's error in map creation
            error({excess_data, MapLeft})
    end.

to_thrift_value({struct, struct, {Mod, Name}}, V = #{}, NameFun) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    Acc = erlang:make_tuple(length(StructDef) + 1, undefined, [{1, Mod:record_name(Name)}]),
    to_thrift_struct(StructDef, V, 2, Acc, NameFun);
to_thrift_value({set, Type}, Vs, NameFun) ->
    ordsets:from_list([to_thrift_value(Type, V, NameFun) || V <- ordsets:to_list(Vs)]);
to_thrift_value(string, V, _NameFun) ->
    V;
to_thrift_value(i64, V, _NameFun) ->
    V;
to_thrift_value(i32, V, _NameFun) ->
    V;
to_thrift_value(i16, V, _NameFun) ->
    V;
to_thrift_value(byte, V, _NameFun) ->
    V.

from_thrift_struct(StructDef, Struct) ->
    % NOTE
    % This 2 refers to the first field in a record tuple.
    from_thrift_struct(StructDef, Struct, 2, #{}).

-spec from_thrift_struct([struct_field_info()], tuple(), number(), map()) -> map().
from_thrift_struct([{_, _Req, Type, Name, _Default} | Rest], Struct, Idx, Acc) ->
    Acc1 =
        case element(Idx, Struct) of
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

identity(V) ->
    V.
