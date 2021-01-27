-module(bouncer_context).

-type set(T) :: ordsets:ordset(T).

%% TODO
%% Currently there's no notion of generic maps in contexts. This means for example that (given
%% `context_v1` model) client may submits fragments like these:
%% ```
%% "frag1": {user: {id: "bla", orgs: [{id: "org42", owner: {id: "blarg"}}]}}
%% "frag2": {user: {id: "bla", orgs: [{id: "org42", owner: {id: "goodwyn"}}]}}
%% ```
%% Which we won't consider conflicting, when they're obviously referring to the same
%% «organization».

-type value() :: number() | binary() | ctx() | set(ctx()) | json().
-type ctx() :: #{atom() => value()}.

-type json() :: null | boolean() | number() | binary() | [json()] | #{binary() => json()}.

-export([empty/0]).
-export([merge/2]).

-export_type([ctx/0]).
-export_type([value/0]).

%%

-spec empty() -> ctx().
empty() ->
    #{}.

-spec merge(ctx(), ctx()) -> {_Merged :: ctx(), _Conflicting :: ctx() | undefined}.
merge(Ctx1, Ctx2) ->
    maps:fold(
        fun(K, V2, {CtxAcc, ConflictAcc}) ->
            case maps:find(K, CtxAcc) of
                {ok, V1} ->
                    {VM, Conflict} = merge_values(V1, V2),
                    CtxAcc1 = CtxAcc#{K => VM},
                    DiscardAcc1 = append_conflict(K, Conflict, ConflictAcc),
                    {CtxAcc1, DiscardAcc1};
                error ->
                    {CtxAcc#{K => V2}, ConflictAcc}
            end
        end,
        {Ctx1, undefined},
        Ctx2
    ).

merge_values(V1 = #{}, V2 = #{}) ->
    merge(V1, V2);
merge_values(V1, V2) ->
    case ordsets:is_set(V1) and ordsets:is_set(V2) of
        true ->
            Intersection = ordsets:intersection(V1, V2),
            MaybeConflict =
                case ordsets:size(Intersection) of
                    0 -> undefined;
                    _ -> Intersection
                end,
            {ordsets:union(V1, V2), MaybeConflict};
        false when V1 =:= V2 ->
            {V2, undefined};
        false ->
            {V2, V1}
    end.

append_conflict(_, undefined, Acc) ->
    Acc;
append_conflict(K, Conflict, undefined) ->
    append_conflict(K, Conflict, #{});
append_conflict(K, Conflict, Acc) ->
    Acc#{K => Conflict}.

%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.
-spec merge_test_() -> [_TestGen].

merge_test_() ->
    C1 = #{lol => #{yeah => 1337}},
    [
        ?_assertEqual({empty(), undefined}, merge(empty(), empty())),
        ?_assertEqual({C1, undefined}, merge(empty(), C1)),
        ?_assertEqual({C1, undefined}, merge(C1, empty()))
    ].

-endif.
