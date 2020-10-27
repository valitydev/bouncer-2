-module(ct_helper).

-export([with_config/3]).

-type config() :: [{atom(), term()}].

-export_type([config/0]).

%%

-spec with_config(atom(), config(), fun ((_) -> R)) ->
    R | undefined.
with_config(Name, C, Fun) ->
    case lists:keyfind(Name, 1, C) of
        {_, V} -> Fun(V);
        false  -> undefined
    end.
