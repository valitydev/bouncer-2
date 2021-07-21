-module(ct_hay_publisher).

-behaviour(hay_metrics_publisher).

%% hay_metrics_publisher callbacks
-export([init/1]).
-export([get_interval/1]).
-export([publish_metrics/2]).

%% API
-export([get_metric/1]).

%% Types

-type options() :: #{
    interval => timeout()
}.

-export_type([options/0]).

%% Internal types
-define(ETS_NAME, ?MODULE).

-record(state, {
    interval :: timeout(),
    ets :: ets:tid() | atom()
}).

-record(metric, {
    key :: how_are_you:metric_key(),
    value :: how_are_you:metric_value()
}).

-type state() :: #state{}.

%% API

-spec init(options()) -> {ok, state()}.
init(Options) ->
    {ok, #state{
        interval = maps:get(interval, Options, 100),
        ets = ets:new(?ETS_NAME, [named_table, set, {keypos, #metric.key}])
    }}.

-spec get_interval(state()) -> timeout().
get_interval(#state{interval = Interval}) ->
    Interval.

-spec publish_metrics(hay_metrics_publisher:metric_fold(), state()) ->
    {ok, state()} | {error, Reason :: term()}.
publish_metrics(Fold, #state{ets = Ets} = State) ->
    true = Fold(
        fun(M, _) ->
            ets:insert(Ets, #metric{key = hay_metrics:key(M), value = hay_metrics:value(M)})
        end,
        true
    ),
    {ok, State}.

-spec get_metric(how_are_you:metric_key()) -> how_are_you:metric_value() | undefined.
get_metric(Key) ->
    % Convert key to hay internal format
    EKey = hay_metrics:key(how_are_you:metric_construct(gauge, Key, 0)),
    case ets:lookup(?ETS_NAME, EKey) of
        [#metric{value = Value}] ->
            Value;
        [] ->
            undefined
    end.
