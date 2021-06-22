-module(bouncer_gunner_event_h).

-include_lib("gunner/include/gunner_events.hrl").

%% gunner_event_h behaviour

-behaviour(gunner_event_h).

-export([handle_event/2]).

%% Internal types

-type state() :: gunner_event_h:state().

-type metric() :: how_are_you:metric().
-type metrics() :: [metric()].
-type metric_key() :: how_are_you:metric_key().
-type bin() :: {number(), Name :: binary()}.

%%
%% gunner_event_h behaviour
%%

-spec handle_event(gunner_event_h:event(), state()) -> state().
handle_event(Event, State) ->
    {Metric, State1} = create_metric(Event, State),
    ok = push_metric(Metric),
    State1.

%%
%% Metrics
%%

-define(INC, 1).
-define(DEC, -1).

create_metric(#gunner_pool_init_event{}, _State) ->
    [];
create_metric(#gunner_pool_terminate_event{}, _State) ->
    [];
%%
create_metric(
    #gunner_acquire_started_event{
        group_id = GroupID,
        client = ClientID,
        is_locking = _IsLocking
    },
    State
) ->
    {ok, State1} = start_timer([acquire, encode_group(GroupID), encode_client(ClientID)], State),
    {
        [
            create_counter([gunner, acquire, started, group, encode_group(GroupID)], ?INC)
        ],
        State1
    };
create_metric(
    #gunner_acquire_finished_event{
        group_id = GroupID,
        client = ClientID,
        result = Result,
        connection = _ConnectionPid
    },
    State
) ->
    {ok, Elapsed, State1} = stop_timer(
        [acquire, encode_group(GroupID), encode_client(ClientID)],
        State
    ),
    {
        [
            create_counter([gunner, acquire, finished, encode_result(Result), group, encode_group(GroupID)], ?INC),
            create_duration([gunner, duration, acquire], Elapsed)
        ],
        State1
    };
%%
create_metric(#gunner_connection_locked_event{group_id = GroupID}, State) ->
    {
        [
            create_counter([gunner, connections, locked, group, encode_group(GroupID)], ?INC)
        ],
        State
    };
create_metric(#gunner_connection_unlocked_event{group_id = GroupID}, State) ->
    {
        [
            create_counter([gunner, connections, locked, group, encode_group(GroupID)], ?DEC)
        ],
        State
    };
%%
create_metric(
    #gunner_free_started_event{
        connection = ConnectionID,
        group_id = GroupID,
        client = ClientID
    },
    State
) ->
    {ok, State1} = start_timer(
        [free, encode_connection(ConnectionID), encode_group(GroupID), encode_client(ClientID)],
        State
    ),
    {
        [
            create_counter([gunner, free, started, group, encode_group(GroupID)], ?INC)
        ],
        State1
    };
create_metric(
    #gunner_free_finished_event{
        connection = ConnectionID,
        group_id = GroupID,
        client = ClientID
    },
    State
) ->
    {ok, Elapsed, State1} = stop_timer(
        [free, encode_connection(ConnectionID), encode_group(GroupID), encode_client(ClientID)],
        State
    ),
    {
        [
            create_counter([gunner, free, finished, group, encode_group(GroupID)], ?INC),
            create_duration([gunner, duration, free], Elapsed)
        ],
        State1
    };
create_metric(#gunner_free_error_event{}, State) ->
    {
        [
            create_counter([gunner, free, error], ?INC),
        ],
        State
    };
%%
create_metric(#gunner_cleanup_started_event{active_connections = Active}, State) ->
    {ok, State1} = start_timer([cleanup], State),
    {[create_gauge([gunner, connections, active], Active)], State1};
create_metric(#gunner_cleanup_finished_event{active_connections = Active}, State) ->
    {ok, Elapsed, State1} = stop_timer([cleanup], State),
    {
        [
            create_gauge([gunner, connections, active], Active),
            create_duration([gunner, duration, cleanup], Elapsed)
        ],
        State1
    };
%%
create_metric(#gunner_client_down_event{}, State) ->
    {
        [
            create_counter([gunner, client, down], ?INC),
        ],
        State
    };
%%
create_metric(
    #gunner_connection_init_started_event{connection = ConnectionID, group_id = GroupID},
    State
) ->
    {ok, State1} = start_timer(
        [connection_init, encode_connection(ConnectionID), encode_group(GroupID)],
        State
    ),
    {
        [
            create_counter([gunner, connection, init, started, group, encode_group(GroupID)], ?INC)
        ],
        State1
    };
create_metric(
    #gunner_connection_init_finished_event{
        connection = ConnectionID,
        group_id = GroupID,
        result = ok
    },
    State
) ->
    {ok, Elapsed, State1} = stop_timer(
        [connection_init, encode_connection(ConnectionID), encode_group(GroupID)],
        State
    ),
    {
        [
            create_counter([gunner, connection, init, finished, ok, group, encode_group(GroupID)], ?INC)
            create_counter([gunner, connections, total, group, encode_group(GroupID)], ?INC),
            create_duration([gunner, duration, connection, init], Elapsed)
        ],
        State1
    };
create_metric(
    #gunner_connection_init_finished_event{
        connection = ConnectionID,
        group_id = GroupID
    },
    State
) ->
    {ok, Elapsed, State1} = stop_timer(
        [connection_init, encode_connection(ConnectionID), encode_group(GroupID)],
        State
    ),
    {
        [
            create_counter([gunner, connection, init, finished, error, group, encode_group(GroupID)], ?INC),
            create_duration([gunner, duration, connection, init], Elapsed)
        ],
        State1
    };
%%
create_metric(#gunner_connection_down_event{group_id = GroupID}, State) ->
    {
        [
            create_counter([gunner, connection, down, group, encode_group(GroupID)], ?INC),
            create_counter([gunner, connections, total, group, encode_group(GroupID)], ?DEC)
        ],
        State
    }.

%%
%% Internal
%%

encode_connection(ConnectionID) ->
    ConnectionID.

encode_group({Host, Port}) ->
    [Host, Port].

encode_client(ClientID) ->
    ClientID.

encode_result(ok) ->
    ok;
encode_result({error, pool_unavailable}) ->
    pool_unavailable;
encode_result({error, {connection_failed, _Reason}}) ->
    connection_failed.

start_timer(TimerKey, State) ->
    Time = erlang:monotonic_time(millisecond),
    Timers = maps:get(timers, State, #{}),
    {ok, State#{timers => Timers#{make_timer_key(TimerKey) => Time}}}.

stop_timer(TimerKey0, State) ->
    Time = erlang:monotonic_time(millisecond),
    Timers = maps:get(timers, State, #{}),
    TimerKey = make_timer_key(TimerKey0),
    case maps:get(TimerKey, Timers, undefined) of
        TimeStarted when TimeStarted =/= undefined ->
            {ok, Time - TimeStarted, State#{timers => maps:remove(TimerKey, Timers)}};
        undefined ->
            {error, no_timer}
    end.

make_timer_key(Key) when is_list(Key) ->
    lists:flatten(Key).

%%
%% Hay utils
%%

-spec push_metric(metrics()) -> ok.

push_metric([]) ->
    ok;
push_metric([M | Metrics]) ->
    ok = how_are_you:metric_push(M),
    push_metric(Metrics).

-spec create_counter(metric_key(), non_neg_integer()) -> metric().
create_counter(Key, Number) ->
    how_are_you:metric_construct(counter, Key, Number).

-spec create_gauge(metric_key(), non_neg_integer()) -> metric().
create_gauge(Key, Number) ->
    how_are_you:metric_construct(gauge, Key, Number).

-spec create_duration(metric_key(), non_neg_integer()) -> metric().
create_duration(KeyPrefix, Duration) ->
    BinKey = build_bin_key(build_bins(), Duration),
    how_are_you:metric_construct(counter, [KeyPrefix, BinKey], 1).

%%

-spec build_bin_key(Bins :: [bin()], Value :: number()) -> metric_key().
build_bin_key([{HeadValue, HeadName} | _Bins], Value) when HeadValue > Value ->
    <<"less_than_", HeadName/binary>>;
build_bin_key([{LastValue, LastName}], Value) when LastValue =< Value ->
    <<"greater_than_", LastName/binary>>;
build_bin_key([{LeftValue, LeftName}, {RightValue, RightName} | _Bins], Value) when
    LeftValue =< Value andalso RightValue > Value
->
    <<"from_", LeftName/binary, "_to_", RightName/binary>>;
build_bin_key([{HeadValue, _HeadName} | Bins], Value) when HeadValue =< Value ->
    build_bin_key(Bins, Value).

-spec build_bins() -> [bin()].
build_bins() ->
    [
        {1000, <<"1ms">>},
        {5 * 1000, <<"5ms">>},
        {10 * 1000, <<"10ms">>},
        {25 * 1000, <<"25ms">>},
        {50 * 1000, <<"50ms">>},
        {100 * 1000, <<"100ms">>},
        {250 * 1000, <<"250ms">>},
        {500 * 1000, <<"500ms">>},
        {1000 * 1000, <<"1s">>},
        {10 * 1000 * 1000, <<"10s">>},
        {30 * 1000 * 1000, <<"30s">>},
        {60 * 1000 * 1000, <<"1m">>},
        {5 * 60 * 1000 * 1000, <<"5m">>}
    ].
