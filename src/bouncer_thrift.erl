-module(bouncer_thrift).

%% API
-export([json_to_thrift/2]).
-export([to_thrift/2]).
-export([to_thrift/3]).

-export([from_thrift/2]).
-export([from_thrift_struct/3]).
-export([from_thrift_struct/4]).

%% Subset of `jsx:json_term()`.
-type generic_term(T) ::
    [generic_term(T)]
    | generic_object(T)
    | true
    | false
    | null
    | number()
    | binary()
    | atom().

-type generic_object(T) :: #{T => generic_term(T)}.

-type context_term() :: generic_term(atom()).
-type context_object() :: generic_object(atom()).
-type json_term() :: generic_term(binary()).

-type thrift_term() :: term().

-type struct_flavour() :: struct | exception | union.

-type field_num() :: pos_integer().
-type field_name() :: atom().
-type field_req() :: required | optional | undefined.

-type type_ref() :: {module(), atom()}.
-type thrift_type() ::
    bool
    | byte
    | i16
    | i32
    | i64
    | string
    | double
    | {enum, type_ref()}
    | {struct, struct_flavour(), type_ref()}
    | {list, thrift_type()}
    | {set, thrift_type()}
    | {map, thrift_type(), thrift_type()}.

-type struct_field_info() ::
    {field_num(), field_req(), thrift_type(), field_name(), any()}.

-type thrift_mapper() :: fun((thrift_type(), thrift_term()) -> {ok, context_term()} | undefined).
-type context_mapper() :: fun((thrift_type(), context_term()) -> {ok, thrift_term()} | undefined).

-spec json_to_thrift(thrift_type(), json_term()) -> thrift_term().
json_to_thrift(Type, V) ->
    to_thrift_value(Type, V, fun genlib:to_binary/1, undefined).

-spec to_thrift(thrift_type(), context_term()) -> thrift_term().
to_thrift(Type, V) ->
    to_thrift_value(Type, V, fun identity/1, undefined).

-spec to_thrift(thrift_type(), context_term(), context_mapper() | undefined) -> thrift_term().
to_thrift(Type, V, Mapper) ->
    to_thrift_value(Type, V, fun identity/1, Mapper).

to_thrift_struct([{_Tag, _Req, Type, Name, Default} | Rest], Map, Idx, Acc, NameFun, Mapper) ->
    TransformedName = NameFun(Name),
    case maps:take(TransformedName, Map) of
        {V, MapLeft} ->
            Acc1 = erlang:setelement(Idx, Acc, to_thrift_value(Type, V, NameFun, Mapper)),
            to_thrift_struct(Rest, MapLeft, Idx + 1, Acc1, NameFun, Mapper);
        error when Default /= undefined ->
            Acc1 = erlang:setelement(Idx, Acc, Default),
            to_thrift_struct(Rest, Map, Idx + 1, Acc1, NameFun, Mapper);
        error ->
            to_thrift_struct(Rest, Map, Idx + 1, Acc, NameFun, Mapper)
    end;
to_thrift_struct([], MapLeft, _Idx, Acc, _NameFun, _Mapper) ->
    case map_size(MapLeft) of
        0 ->
            Acc;
        _ ->
            %% Some part of map was left after converting to thrift,
            %% indicating that either thrift structure doesn't have
            %% enough fields or there's error in map creation
            error({excess_data, MapLeft})
    end.

to_thrift_value(Type, V, NameFun, undefined) ->
    to_thrift_value_def(Type, V, NameFun, undefined);
to_thrift_value(Type, V, NameFun, Mapper) ->
    case Mapper(Type, V) of
        {ok, R} ->
            R;
        undefined ->
            to_thrift_value_def(Type, V, NameFun, Mapper)
    end.

to_thrift_value_def({struct, struct, {Mod, Name}}, V = #{}, NameFun, Mapper) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    Acc = erlang:make_tuple(length(StructDef) + 1, undefined, [{1, Mod:record_name(Name)}]),
    % NOTE
    % This 2 refers to the first field in a record tuple.
    to_thrift_struct(StructDef, V, 2, Acc, NameFun, Mapper);
to_thrift_value_def({set, Type}, Vs, NameFun, Mapper) ->
    ordsets:from_list([to_thrift_value(Type, V, NameFun, Mapper) || V <- ordsets:to_list(Vs)]);
to_thrift_value_def(string, V, _NameFun, _) ->
    V;
to_thrift_value_def(i64, V, _NameFun, _) ->
    V;
to_thrift_value_def(i32, V, _NameFun, _) ->
    V;
to_thrift_value_def(i16, V, _NameFun, _) ->
    V;
to_thrift_value_def(byte, V, _NameFun, _) ->
    V.

-spec from_thrift(thrift_type(), thrift_term()) -> context_term().
from_thrift(Type, V) ->
    from_thrift_value(Type, V, undefined).

-spec from_thrift_struct([struct_field_info()], tuple(), number()) -> context_object().
from_thrift_struct(StructDef, Struct, Idx) ->
    from_thrift_struct(StructDef, Struct, Idx, undefined, #{}).

-spec from_thrift_struct([struct_field_info()], tuple(), number(), thrift_mapper() | undefined) ->
    context_object().
from_thrift_struct(StructDef, Struct, Idx, Mapper) ->
    from_thrift_struct(StructDef, Struct, Idx, Mapper, #{}).

from_thrift_struct([{_, _Req, Type, Name, _Default} | Rest], Struct, Idx, Mapper, Acc) ->
    Acc1 =
        case element(Idx, Struct) of
            V when V /= undefined ->
                Acc#{Name => from_thrift_value(Type, V, Mapper)};
            undefined ->
                Acc
        end,
    from_thrift_struct(Rest, Struct, Idx + 1, Mapper, Acc1);
from_thrift_struct([], _Struct, _, _, Acc) ->
    Acc.

from_thrift_value(Type, V, undefined) ->
    from_thrift_value_def(Type, V, undefined);
from_thrift_value(Type, V, Mapper) ->
    case Mapper(Type, V) of
        {ok, R} ->
            R;
        undefined ->
            from_thrift_value_def(Type, V, Mapper)
    end.

from_thrift_value_def({struct, struct, {Mod, Name}}, V, Mapper) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    % NOTE
    % This 2 refers to the first field in a record tuple.
    from_thrift_struct(StructDef, V, 2, Mapper);
from_thrift_value_def({set, Type}, Vs, Mapper) ->
    ordsets:from_list([from_thrift_value(Type, V, Mapper) || V <- ordsets:to_list(Vs)]);
from_thrift_value_def(string, V, _) ->
    V;
from_thrift_value_def(i64, V, _) ->
    V;
from_thrift_value_def(i32, V, _) ->
    V;
from_thrift_value_def(i16, V, _) ->
    V;
from_thrift_value_def(byte, V, _) ->
    V.

identity(V) ->
    V.
