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
  init_metric/2,
  update/1,
  update/2,
  header/0,
  html_header/0,
  display/3,
  html_display/3,
  oneup_max/2]).


%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(DISPLAY_FORMAT,"~-15s~-50s~-20w~-20.4f~-20.4f~-20.4f~-20.4f~-20.4f~-20.4f~-20.4f~n").
-define(HTML_DISPLAY_FORMAT, "<tr><td><b>~-15s</b></td><td><b>~-50s</b></td><td>~-20w</td><td>~-20.4f</td><td>~-20.4f</td><td>~-20.4f</td><td>~-20.4f</td><td>~-20.4f</td><td>~-20.4f</td><td>~-20.4f</td></tr>").

-define(INTERVAL, 5).
-define(SECONDS_PER_MINUTE, 60.0).

-define(INTERVAL_MILLIS, 5000).
-define(ONE_MINUTE_MILLIS, 60 * 1000).
-define(FIVE_MINUTE_MILLIS, ?ONE_MINUTE_MILLIS * 5).
-define(FIFTEEN_MINUTE_MILLIS, ?ONE_MINUTE_MILLIS * 15).
-define(HOUR_MINUTES, 60).
-define(DAY_MINUTES, ?HOUR_MINUTES * 24).

-record(state, {
  domain = [],
  display_name,
  counter,
  start,
  lifetime_total = 0,
  instant_rate = 0.0,
  one_minute_rate = 0.0,
  five_minute_rate = 0.0,
  fifteen_minute_rate = 0.0,
  hour_rate = 0.0,
  day_rate = 0.0}).



%%%===================================================================
%%% API
%%%===================================================================

start_link(MetricName, CounterRef) when is_atom(MetricName) ->
  gen_server:start_link({local, MetricName}, ?MODULE, [MetricName, CounterRef], []).

%%%===================================================================
%%% oneup_metrics callbacks
%%%===================================================================

init_metric(MetricName) when is_list(MetricName)->
  init_metric([], MetricName);
init_metric(MetricName) when is_atom(MetricName) ->
  Counter = oneup:new_counter(),
  oneup_meter_sup:start_meter(MetricName, Counter),
  {?MODULE, MetricName, Counter}.

init_metric(Domain, MetricName) when is_atom(Domain)->
  init_metric([Domain], MetricName);
init_metric(Domain, MetricName) when is_list(Domain), is_list(MetricName)->
  MetricNameAtom = oneup_metrics:metric_name_to_atom(Domain ++ MetricName),
  init_metric(MetricNameAtom).

update(CounterRef)->
  oneup:inc(CounterRef).

update(CounterRef, Value) when is_integer(Value) ->
  oneup:inc2(CounterRef, Value).

html_header()->
  "<tr><td><b>meter</b></td><td> </td><td>count</td><td>mean</td><td>cur_rate</td><td>1m_rate</td><td>5m_rate</td><td>15m_rate</td><td>1h_rate</td><td>day_rate<</td></tr>".
header()->
  lists:flatten(io_lib:format("~-15s~-50s~-20s~-20s~-20s~-20s~-20s~-20s~-20s~-20s~n",
    ["meter", "", "count", "mean", "cur_rate", "1m_rate", "5m_rate", "15m_rate", "1h_rate", "day_rate"])).

html_display(DisplayName, Domain, CounterValue) ->
  do_display(DisplayName, Domain, CounterValue, ?HTML_DISPLAY_FORMAT).

display(DisplayName, Domain, CounterValue) ->
  do_display(DisplayName, Domain, CounterValue, ?DISPLAY_FORMAT).

do_display(DisplayName, Domain, CounterValue, DisplayFormat) when is_atom(DisplayName) ->
  do_display(atom_to_list(DisplayName), Domain, CounterValue, DisplayFormat);
do_display(DisplayName, Domain, CounterRef, DisplayFormat) when is_reference(CounterRef) ->
  CounterVal = oneup:get(CounterRef),
  lists:flatten(io_lib:format(DisplayFormat,
    ["meter (ERR)", oneup_metrics:display_metric_name(DisplayName, Domain), CounterVal, 0,0,0,0,0,0,0]));
do_display(DisplayName, Domain, {Counter, Mean, InstantRate, OneMinRate, FiveMinRate, FifteenMinRate, HourRate, DayRate}, DisplayFormat)
  when is_list(DisplayName), is_list(Domain)->
    lists:flatten(io_lib:format(DisplayFormat,
      ["meter", oneup_metrics:display_metric_name(DisplayName, Domain), Counter, Mean, InstantRate, OneMinRate, FiveMinRate, FifteenMinRate, HourRate, DayRate])).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MetricName, CounterRef]) ->
  erlang:start_timer(?INTERVAL_MILLIS, self(), tick),
  {ok, #state{display_name = atom_to_list(MetricName), counter = CounterRef, start = oneup_metrics:current_second()}}.

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
  Mean = LifetimeTotal / oneup_max(oneup_metrics:current_second() - Start, 1),
  Ret = [Counter, Mean, InstantRate, OneMinRate, FiveMinRate, FifteenMinRate, HourRate, DayRate],
  {reply, Ret, State};

handle_call({display, Domain}, _From, State)->
  do_display_running(Domain, State, display);

handle_call({html_display, Domain}, _From, State)->
  do_display_running(Domain, State, html_display).


do_display_running(Domain,  #state{counter = CounterRef,
                                    display_name = DisplayName,
                                    start = Start,
                                    lifetime_total = LifetimeTotal,
                                    instant_rate = InstantRate,
                                    one_minute_rate = OneMinRate,
                                    five_minute_rate = FiveMinRate,
                                    fifteen_minute_rate = FifteenMinRate,
                                    hour_rate = HourRate,
                                    day_rate = DayRate} = State, DisplayMethod) ->
  Counter = oneup:get(CounterRef),
  Mean =
  case (oneup_metrics:current_second() - Start) of
    Duration when Duration =/= 0 -> LifetimeTotal / Duration;
    Duration when Duration =:= 0 -> 0
  end,
  DisplayMeterValues = ?MODULE:DisplayMethod(DisplayName, Domain,
    {Counter, Mean, InstantRate, OneMinRate, FiveMinRate, FifteenMinRate, HourRate, DayRate}),
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
  erlang:start_timer(?INTERVAL_MILLIS, self(), tick),
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

%% TODO alpha calculation should be configurable.
%% Original based on dropwizard was
%% 1 - math:exp(-?INTERVAL / ?SECONDS_PER_MINUTE / Minutes).
%% But we liked more accuracy for the recent occurences, specifically 1-minute rate,
%% hence the modification

oneup_max(A, B) when A > B -> A;
oneup_max(A, B) when A =< B -> B.

alpha(Minutes)->
  1 - math:exp(-math:pow(?INTERVAL,2) / ?SECONDS_PER_MINUTE / math:pow(Minutes,2)).

tick(_Minutes, Count, undefined)->
  Count / ?INTERVAL;  %% just return instant rate
tick(Minutes, Count, PrevRate)->
  InstantRate = Count / ?INTERVAL,
  PrevRate + (alpha(Minutes) * (InstantRate - PrevRate)).
