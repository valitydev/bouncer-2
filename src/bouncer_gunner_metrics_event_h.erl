-module(bouncer_gunner_metrics_event_h).

-include_lib("gunner/include/gunner_events.hrl").

%% gunner_event_h behaviour

-behaviour(gunner_event_h).

-export([handle_event/2]).

%% Internal types

-type state() :: #{timer_id() => timer_start_ts()}.

-type timer_id() :: term().
-type timer_start_ts() :: non_neg_integer().
-type metric_key() :: how_are_you:metric_key().

%%
%% gunner_event_h behaviour
%%

-spec handle_event(gunner_event_h:event(), state()) -> state().
handle_event(Event, State) ->
    try
        ok = create_metric(Event),
        process_timers(Event, State)
    catch
        throw:{stop_timer_failed, {no_timer, TimerKey}} ->
            _ = logger:error("Tried to stop a non-existant timer: ~p", [TimerKey]),
            State
    end.

%%
%% Metrics
%%

-define(METRIC_KEY(Tag, Content), [gunner, Tag, Content]).

-define(METRIC_DURATION(Key), ?METRIC_KEY(duration, Key)).
-define(METRIC_ACQUIRE(Evt), ?METRIC_KEY(acquire, Evt)).
-define(METRIC_FREE(Evt), ?METRIC_KEY(free, Evt)).
-define(METRIC_LOCK(Evt), ?METRIC_KEY(lock, Evt)).
-define(METRIC_CONNECTION(Evt), ?METRIC_KEY(connection, Evt)).

-define(TIMER_KEY(Tag, Content), {Tag, Content}).
-define(TIMER_CLEANUP, cleanup).
-define(TIMER_ACQUIRE(GroupID, ClientID),
    ?TIMER_KEY(acquire, {GroupID, ClientID})
).
-define(TIMER_FREE(ConnectionID, GroupID, ClientID),
    ?TIMER_KEY(free, {ConnectionID, GroupID, ClientID})
).
-define(TIMER_CONNECTION_INIT(ConnectionID, GroupID),
    ?TIMER_KEY(connection_init, {ConnectionID, GroupID})
).

process_timers(Event, State) ->
    case is_timed_event(Event) of
        {true, {start, TimerID}} ->
            start_timer(TimerID, State);
        {true, {finish, TimerID, MetricID}} ->
            {Elapsed, State1} = stop_timer(TimerID, State),
            ok = create_duration(MetricID, Elapsed),
            State1;
        false ->
            State
    end.

is_timed_event(#gunner_acquire_started_event{group_id = GroupID, client = ClientID}) ->
    {true, {start, ?TIMER_ACQUIRE(GroupID, ClientID)}};
is_timed_event(#gunner_acquire_finished_event{group_id = GroupID, client = ClientID}) ->
    {true, {finish, ?TIMER_ACQUIRE(GroupID, ClientID), ?METRIC_DURATION(acquire)}};
is_timed_event(#gunner_free_started_event{
    group_id = GroupID,
    client = ClientID,
    connection = ConnectionID
}) ->
    {true, {start, ?TIMER_FREE(ConnectionID, GroupID, ClientID)}};
is_timed_event(#gunner_free_finished_event{
    group_id = GroupID,
    client = ClientID,
    connection = ConnectionID
}) ->
    {true, {finish, ?TIMER_FREE(ConnectionID, GroupID, ClientID), ?METRIC_DURATION(free)}};
is_timed_event(#gunner_cleanup_started_event{}) ->
    {true, {start, ?TIMER_CLEANUP}};
is_timed_event(#gunner_cleanup_finished_event{}) ->
    {true, {finish, ?TIMER_CLEANUP, ?METRIC_DURATION(cleanup)}};
is_timed_event(
    #gunner_connection_init_started_event{
        group_id = GroupID,
        connection = ConnectionID
    }
) ->
    {true, {start, ?TIMER_CONNECTION_INIT(GroupID, ConnectionID)}};
is_timed_event(
    #gunner_connection_init_finished_event{
        group_id = GroupID,
        connection = ConnectionID
    }
) ->
    {true,
        {finish, ?TIMER_CONNECTION_INIT(GroupID, ConnectionID), ?METRIC_DURATION(connection_init)}};
is_timed_event(_) ->
    false.

create_metric(#gunner_pool_init_event{pool_opts = PoolOpts}) ->
    ok = create_gauge(?METRIC_KEY(config, [connections, max]), maps:get(max_size, PoolOpts)),
    create_gauge(?METRIC_KEY(config, [connections, min]), maps:get(min_size, PoolOpts));
create_metric(#gunner_pool_terminate_event{}) ->
    ok;
%%
create_metric(#gunner_acquire_started_event{}) ->
    counter_inc(?METRIC_ACQUIRE(started));
create_metric(#gunner_acquire_finished_event{result = Result}) ->
    counter_inc(?METRIC_ACQUIRE([finished, encode_result(Result)]));
%%
create_metric(#gunner_connection_locked_event{}) ->
    counter_inc(?METRIC_LOCK(locked));
create_metric(#gunner_connection_unlocked_event{}) ->
    counter_inc(?METRIC_LOCK(unlocked));
%%
create_metric(#gunner_free_started_event{}) ->
    counter_inc(?METRIC_FREE(started));
create_metric(#gunner_free_finished_event{}) ->
    counter_inc(?METRIC_FREE(finished));
create_metric(#gunner_free_error_event{}) ->
    counter_inc([gunner, free, error]);
%%
create_metric(#gunner_cleanup_started_event{active_connections = Active}) ->
    create_gauge(?METRIC_KEY(connections, [cleanup, before]), Active);
create_metric(#gunner_cleanup_finished_event{active_connections = Active}) ->
    create_gauge(?METRIC_KEY(connections, [cleanup, 'after']), Active);
%%
create_metric(#gunner_client_down_event{}) ->
    counter_inc([gunner, client, down]);
%%
create_metric(#gunner_connection_init_started_event{}) ->
    counter_inc(?METRIC_CONNECTION([init, started]));
%%
create_metric(#gunner_connection_init_finished_event{result = ok}) ->
    counter_inc(?METRIC_CONNECTION([init, finished, ok]));
create_metric(#gunner_connection_init_finished_event{result = _}) ->
    counter_inc(?METRIC_CONNECTION([init, finished, error]));
%%
create_metric(#gunner_connection_down_event{reason = Reason}) ->
    counter_inc(?METRIC_CONNECTION([down, encode_down_reason(Reason)])).

%%
%% Internal
%%

encode_down_reason(normal) ->
    normal;
encode_down_reason({abnormal, _}) ->
    abnormal.

encode_result(ok) ->
    ok;
encode_result({error, pool_unavailable}) ->
    pool_unavailable;
encode_result({error, {connection_failed, _Reason}}) ->
    connection_failed.

start_timer(TimerKey, State) ->
    Time = erlang:monotonic_time(microsecond),
    State#{TimerKey => Time}.

stop_timer(TimerKey, State) ->
    Time = erlang:monotonic_time(microsecond),
    case maps:get(TimerKey, State, undefined) of
        TimeStarted when TimeStarted =/= undefined ->
            {Time - TimeStarted, maps:remove(TimerKey, State)};
        undefined ->
            throw({stop_timer_failed, {no_timer, TimerKey}})
    end.

%%
%% Hay utils
%%

-spec counter_inc(metric_key()) -> ok.
counter_inc(Key) ->
    create_counter(Key, 1).

-spec create_counter(metric_key(), integer()) -> ok.
create_counter(Key, Number) ->
    create_metric(counter, Key, Number).

-spec create_gauge(metric_key(), non_neg_integer()) -> ok.
create_gauge(Key, Number) ->
    create_metric(gauge, Key, Number).

-spec create_duration(metric_key(), non_neg_integer()) -> ok.
create_duration(KeyPrefix, Duration) ->
    BinKey = build_bin_key(Duration),
    create_metric(counter, [KeyPrefix, BinKey], 1).

-spec create_metric(atom(), metric_key(), integer()) -> ok.
create_metric(Type, Key, Value) ->
    Metric = how_are_you:metric_construct(Type, Key, Value),
    how_are_you:metric_push(Metric).

%%

-define(_10US, "10us").
-define(_50US, "50us").
-define(_100US, "100us").
-define(_500US, "500us").
-define(_1MS, "1ms").
-define(_5MS, "5ms").
-define(_10MS, "10ms").
-define(_25MS, "25ms").
-define(_50MS, "50ms").
-define(_100MS, "100ms").
-define(_250MS, "250ms").
-define(_500MS, "500ms").
-define(_1S, "1s").
-define(_5S, "5s").

-define(BETWEEN(Bin1, Bin2), <<"from_", Bin1, "_to_", Bin2>>).
-define(LT(Bin), <<"less_than_", Bin>>).
-define(GT(Bin), <<"greater_than_", Bin>>).

-spec build_bin_key(Value :: number()) -> metric_key().
build_bin_key(Value) ->
    if
        Value < 10 -> ?LT(?_10US);
        Value < 50 -> ?BETWEEN(?_10US, ?_50US);
        Value < 100 -> ?BETWEEN(?_50US, ?_100US);
        Value < 500 -> ?BETWEEN(?_100US, ?_500US);
        Value < 1000 -> ?BETWEEN(?_500US, ?_1MS);
        Value < 5 * 1000 -> ?BETWEEN(?_1MS, ?_5MS);
        Value < 10 * 1000 -> ?BETWEEN(?_5MS, ?_10MS);
        Value < 25 * 1000 -> ?BETWEEN(?_10MS, ?_25MS);
        Value < 50 * 1000 -> ?BETWEEN(?_25MS, ?_50MS);
        Value < 100 * 1000 -> ?BETWEEN(?_50MS, ?_100MS);
        Value < 250 * 1000 -> ?BETWEEN(?_100MS, ?_250MS);
        Value < 500 * 1000 -> ?BETWEEN(?_250MS, ?_500MS);
        Value < 1000 * 1000 -> ?BETWEEN(?_500MS, ?_1S);
        Value < 5 * 1000 * 1000 -> ?BETWEEN(?_1S, ?_5S);
        true -> ?GT(?_5S)
    end.
