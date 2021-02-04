-module(ct_helper).

-export([with_config/3]).

-export([get_temp_dir/0]).
-export([get_env/1]).

-type config() :: [{atom(), term()}].

-export_type([config/0]).

%%

-spec with_config(atom(), config(), fun((_) -> R)) -> R | undefined.
with_config(Name, C, Fun) ->
    case lists:keyfind(Name, 1, C) of
        {_, V} -> Fun(V);
        false -> undefined
    end.

-spec get_temp_dir() -> file:filename_all().
get_temp_dir() ->
    hd(
        genlib_list:compact([
            get_env("TMPDIR"),
            get_env("TEMP"),
            get_env("TMP"),
            "/tmp"
        ])
    ).

-spec get_env(string()) -> string() | undefined.
get_env(Name) ->
    case os:getenv(Name) of
        V when is_list(V) ->
            V;
        false ->
            undefined
    end.
