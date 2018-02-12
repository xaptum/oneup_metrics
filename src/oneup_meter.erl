%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2017 12:17 PM
%%%-------------------------------------------------------------------
-module(oneup_meter).
-author("iguberman").

-behaviour(gen_server).
-behaviour(oneup_metrics).

%% API
-export([start_link/2]).

%% oneup_metrics callbacks
-export([
  init_metric/1,
  update/1,
  update/2,
  header/0]).


%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(INTERVAL, 5).
-define(SECONDS_PER_MINUTE, 60.0).

-define(INTERVAL_MILLIS, 5000).
-define(ONE_MINUTE_MILLIS, 60 * 1000).
-define(FIVE_MINUTE_MILLIS, ?ONE_MINUTE_MILLIS * 5).
-define(FIFTEEN_MINUTE_MILLIS, ?ONE_MINUTE_MILLIS * 15).
-define(HOUR_MINUTES, 60).
-define(DAY_MINUTES, ?HOUR_MINUTES * 24).

-record(state, {
  display_name,
  counter,
  start,
  lifetime_total = 0,
  instant_rate,
  one_minute_rate,
  five_minute_rate,
  fifteen_minute_rate,
  hour_rate,
  day_rate}).



%%%===================================================================
%%% API
%%%===================================================================

start_link(MetricName, CounterRef) ->
  gen_server:start_link({local, oneup_metrics:metric_name_to_atom(MetricName)}, ?MODULE, [MetricName, CounterRef], []).

%%%===================================================================
%%% oneup_metrics callbacks
%%%===================================================================

init_metric(MetricName)->
  Counter = oneup:new_counter(),
  oneup_meter_sup:start_meter(MetricName, Counter),
  {?MODULE, Counter}.

update(CounterRef)->
  oneup:inc(CounterRef).

update(CounterRef, Value) when is_integer(Value) ->
  oneup:inc2(CounterRef, Value).

header()->
  lists:flatten(io_lib:format("~-15s~-50s~-20s~-20s~-20s~-20s~-20s~-20s~-20s~-20s~n",
    ["meter", "", "count", "mean", "cur_rate", "1m_rate", "5m_rate", "15m_rate", "1h_rate", "day_rate"])).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MetricName, CounterRef]) ->
  erlang:start_timer(?INTERVAL_MILLIS, self(), tick),
  DisplayName = oneup_metrics_handler:display_metric_name(MetricName),
  {ok, #state{display_name = DisplayName, counter = CounterRef, start = oneup_metrics:current_second()}}.

handle_call(get, _From, #state{
  counter = CounterRef,
  start = Start,
  lifetime_total = LifetimeTotal,
  instant_rate = InstantRate,
  one_minute_rate = OneMinRate,
  five_minute_rate = FiveMinRate,
  fifteen_minute_rate = FifteenMinRate,
  hour_rate = HourRate,
  day_rate = DayRate} = State) ->
  Counter = oneup:get(CounterRef),
  Mean = LifetimeTotal / (oneup_metrics:current_second() - Start),
  Ret = [Counter, Mean, InstantRate, OneMinRate, FiveMinRate, FifteenMinRate, HourRate, DayRate],
  {reply, Ret, State};
handle_call(display, _From, #state{counter = CounterRef,
                                    display_name = DisplayName,
                                    start = Start,
                                    lifetime_total = LifetimeTotal,
                                    instant_rate = InstantRate,
                                    one_minute_rate = OneMinRate,
                                    five_minute_rate = FiveMinRate,
                                    fifteen_minute_rate = FifteenMinRate,
                                    hour_rate = HourRate,
                                    day_rate = DayRate} = State) ->
  Counter = oneup:get(CounterRef),
  Mean = LifetimeTotal / (oneup_metrics:current_second() - Start),
  DisplayMeterValues = lists:flatten(io_lib:format("~-15s~-50s~-20b~-20e~-20e~-20e~-20e~-20e~-20e~-20e~n",
    ["meter", DisplayName, Counter, Mean, InstantRate, OneMinRate, FiveMinRate, FifteenMinRate, HourRate, DayRate])),
  {reply, DisplayMeterValues, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _TimerRef, tick},
    #state{
      counter = CounterRef,
      lifetime_total = LifetimeTotal,
      one_minute_rate = OneMinuteRate,
      five_minute_rate = FiveMinuteRate,
      fifteen_minute_rate = FifteenMinuteRate,
      hour_rate = HourRate,
      day_rate = DayRate} = State) ->
  Count = oneup:set(CounterRef, 0),
  {noreply, State#state{
    instant_rate = Count / ?INTERVAL,
    lifetime_total = LifetimeTotal + Count,
    one_minute_rate = tick(1, Count, OneMinuteRate),
    five_minute_rate = tick(5, Count, FiveMinuteRate),
    fifteen_minute_rate = tick(15, Count, FifteenMinuteRate),
    hour_rate = tick(?HOUR_MINUTES, Count, HourRate),
    day_rate = tick(?DAY_MINUTES, Count, DayRate)}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

alpha(Minutes)->
  1 - math:exp(-?INTERVAL / ?SECONDS_PER_MINUTE / Minutes).

tick(_Minutes, Count, undefined)->
  Count / ?INTERVAL;  %% just return instant rate
tick(Minutes, Count, PrevRate)->
  InstantRate = Count / ?INTERVAL,
  PrevRate + (alpha(Minutes) * (InstantRate - PrevRate)).
