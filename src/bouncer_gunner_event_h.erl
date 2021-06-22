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
-define(METRIC_KEY(Tag, Content), [gunner, Tag, Content]).
-define(METRIC_KEY(Tag, Content, GroupID), [gunner, Tag, Content, group, encode_group(GroupID)]).

-define(METRIC_DURATION(Key), ?METRIC_KEY(duration, Key)).
-define(METRIC_ACQUIRE(Evt, GroupID), ?METRIC_KEY(acquire, Evt, GroupID)).
-define(METRIC_FREE(Evt, GroupID), ?METRIC_KEY(free, Evt, GroupID)).
-define(METRIC_CONNECTION_COUNT(Category), ?METRIC_KEY(connections, Category)).
-define(METRIC_CONNECTION_COUNT(Category, GroupID), ?METRIC_KEY(connections, Category, GroupID)).
-define(METRIC_CONNECTION(Evt, GroupID), ?METRIC_KEY(connection, Evt, GroupID)).

-define(TIMER_KEY(Tag, Content), {Tag, Content}).
-define(TIMER_ACQUIRE(GroupID, ClientID),
    ?TIMER_KEY(acquire, {GroupID, ClientID})
).
-define(TIMER_FREE(ConnectionID, GroupID, ClientID),
    ?TIMER_KEY(free, {ConnectionID, GroupID, ClientID})
).
-define(TIMER_CONNECTION_INIT(ConnectionID, GroupID),
    ?TIMER_KEY(connection_init, {ConnectionID, GroupID})
).

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
    {ok, State1} = start_timer(?TIMER_ACQUIRE(GroupID, ClientID), State),
    {
        [
            counter_inc(?METRIC_ACQUIRE(started, GroupID))
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
    {ok, Elapsed, State1} = stop_timer(?TIMER_ACQUIRE(GroupID, ClientID), State),
    {
        [
            counter_inc(?METRIC_ACQUIRE([finished, encode_result(Result)], GroupID)),
            create_duration(?METRIC_DURATION(acquire), Elapsed)
        ],
        State1
    };
%%
create_metric(#gunner_connection_locked_event{group_id = GroupID}, State) ->
    {
        [
            counter_inc(?METRIC_CONNECTION_COUNT(locked, GroupID))
        ],
        State
    };
create_metric(#gunner_connection_unlocked_event{group_id = GroupID}, State) ->
    {
        [
            counter_dec(?METRIC_CONNECTION_COUNT(locked, GroupID))
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
    {ok, State1} = start_timer(?TIMER_FREE(ConnectionID, GroupID, ClientID), State),
    {
        [
            counter_inc(?METRIC_FREE(started, GroupID))
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
    {ok, Elapsed, State1} = stop_timer(?TIMER_FREE(ConnectionID, GroupID, ClientID), State),
    {
        [
            counter_inc(?METRIC_FREE(finished, GroupID)),
            create_duration(?METRIC_DURATION(free), Elapsed)
        ],
        State1
    };
create_metric(#gunner_free_error_event{}, State) ->
    {
        [
            counter_inc([gunner, free, error])
        ],
        State
    };
%%
create_metric(#gunner_cleanup_started_event{active_connections = Active}, State) ->
    {ok, State1} = start_timer(cleanup, State),
    {[create_gauge(?METRIC_CONNECTION_COUNT(active), Active)], State1};
create_metric(#gunner_cleanup_finished_event{active_connections = Active}, State) ->
    {ok, Elapsed, State1} = stop_timer(cleanup, State),
    {
        [
            create_gauge(?METRIC_CONNECTION_COUNT(active), Active),
            create_duration(?METRIC_DURATION(cleanup), Elapsed)
        ],
        State1
    };
%%
create_metric(#gunner_client_down_event{}, State) ->
    {
        [
            counter_inc([gunner, client, down])
        ],
        State
    };
%%
create_metric(
    #gunner_connection_init_started_event{
        connection = ConnectionID,
        group_id = GroupID
    },
    State
) ->
    {ok, State1} = start_timer(?TIMER_CONNECTION_INIT(ConnectionID, GroupID), State),
    {
        [
            counter_inc(?METRIC_CONNECTION([init, started], GroupID))
        ],
        State1
    };
create_metric(
    #gunner_connection_init_finished_event{
        connection = ConnectionID,
        group_id = GroupID,
        result = Result
    },
    State
) ->
    {ok, Elapsed, State1} = stop_timer(?TIMER_CONNECTION_INIT(ConnectionID, GroupID), State),
    Metrics =
        case Result of
            ok ->
                [
                    counter_inc(?METRIC_CONNECTION([init, finished, ok], GroupID)),
                    counter_inc(?METRIC_CONNECTION_COUNT(total, GroupID))
                ];
            _ ->
                [counter_inc(?METRIC_CONNECTION([init, finished, error], GroupID))]
        end,
    {
        Metrics ++ [create_duration(?METRIC_DURATION([connection, init]), Elapsed)],
        State1
    };
%%
create_metric(#gunner_connection_down_event{group_id = GroupID}, State) ->
    {
        [
            counter_inc(?METRIC_CONNECTION(down, GroupID)),
            counter_dec(?METRIC_CONNECTION_COUNT(total, GroupID))
        ],
        State
    }.

%%
%% Internal
%%

encode_group({Host, Port}) ->
    [Host, Port].

encode_result(ok) ->
    ok;
encode_result({error, pool_unavailable}) ->
    pool_unavailable;
encode_result({error, {connection_failed, _Reason}}) ->
    connection_failed.

start_timer(TimerKey, State) ->
    Time = erlang:monotonic_time(millisecond),
    Timers = maps:get(timers, State, #{}),
    {ok, State#{timers => Timers#{TimerKey => Time}}}.

stop_timer(TimerKey, State) ->
    Time = erlang:monotonic_time(millisecond),
    Timers = maps:get(timers, State, #{}),
    case maps:get(TimerKey, Timers, undefined) of
        TimeStarted when TimeStarted =/= undefined ->
            {ok, Time - TimeStarted, State#{timers => maps:remove(TimerKey, Timers)}};
        undefined ->
            {error, no_timer}
    end.

%%
%% Hay utils
%%

-spec push_metric(metrics()) -> ok.

push_metric([]) ->
    ok;
push_metric([M | Metrics]) ->
    ok = how_are_you:metric_push(M),
    push_metric(Metrics).

-spec counter_inc(metric_key()) -> metric().
counter_inc(Key) ->
    create_counter(Key, 1).

-spec counter_dec(metric_key()) -> metric().
counter_dec(Key) ->
    create_counter(Key, -1).

-spec create_counter(metric_key(), integer()) -> metric().
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
