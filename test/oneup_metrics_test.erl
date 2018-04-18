%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2017 2:34 PM
%%%-------------------------------------------------------------------
-module(oneup_metrics_test).
-author("iguberman").

-include_lib("eunit/include/eunit.hrl").


-define(INTERVAL, 5).
-define(SECONDS_PER_MINUTE, 60.0).

-define(INTERVAL_MILLIS, 5000).
-define(ONE_MINUTE_MILLIS, 60 * 1000).
-define(FIVE_MINUTE_MILLIS, ?ONE_MINUTE_MILLIS * 5).
-define(FIFTEEN_MINUTE_MILLIS, ?ONE_MINUTE_MILLIS * 15).
-define(HOUR_MINUTES, 60).
-define(DAY_MINUTES, ?HOUR_MINUTES * 24).
%%@@@@@@@@@@@@@@@@@@@@@@@@@@@  EUNIT @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

meter_test_()->
  {timeout, 30, testing_meter()}.


%%oneup_config_test()->
%%  oneup_metric_config:init(),
%%  oneup_metric_config:insert(test_entry_1,[{atom, test}]),
%%  oneup_metric_config:insert(test_entry_2,[{atom, test_0}, {list, [test_1, test_2]}]),
%%  oneup_metric_config:start(),
%%  undefined = oneup_metric_config:get(test_entry_0, atom),
%%  test = oneup_metric_config:get(test_entry_1, atom),
%%  [test_1, test_2] = oneup_metric_config:get(test_entry_2, list).



display_counters_test()->
  StatsConfig = [{oneup_counter,
    [
    [a, b, c1, d1, ref1],
    [a, b, c1, d2, ref2],
    [a, b, c2, d1, ref3],
    [a, b, c2, d1, ref4],
    [a2, b2, c2, d2, ref5]
      ]}
  ],

  application:ensure_all_started(lager),
  application:set_env(oneup_metrics, metrics_config, StatsConfig),
  application:ensure_all_started(oneup_metrics),

  StatsMap = oneup_metrics:init_from_config(StatsConfig),
  Body = oneup_metrics_handler:display_metrics(StatsMap),

  ct:print("@@@@@@@@@@@@ ~nFULL METRICS MAP:~n@@@@@@@@@@@@@@@@@@@@@@@@@@@~n ~p~n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@", [Body]),

  SubMetricsMapA = oneup_metrics:get_sub_metrics(StatsMap, [a]),
  SubMetricsBodyA = oneup_metrics_handler:display_metrics(SubMetricsMapA),
  ct:print("@@@@@@@@@@@@ ~nMETRICS MAP a:~n@@@@@@@@@@@@@@@@@@@@@@@@@@@~n ~p~n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@", [SubMetricsBodyA]),


  SubMetricsMapABC2 = oneup_metrics:get_sub_metrics(StatsMap, [a, b, c2]),
  SubMetricsBodyABC2 = oneup_metrics_handler:display_metrics(SubMetricsMapABC2),
  ct:print("@@@@@@@@@@@@ ~nMETRICS MAP a:~n@@@@@@@@@@@@@@@@@@@@@@@@@@@~n ~p~n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@", [SubMetricsBodyABC2]).

init_from_config_test() ->

  StatsConfig = [
    {oneup_counter,
    [[a, b, c1, d1, ref1],
    [a, b, c1, d2, ref2],
    [a, b, c2, d1, ref3],
    [a, b, c2, d1, ref4],
    [a2, b2, c2, d2, ref5]]}
  ],

  application:ensure_all_started(lager),
  application:set_env(oneup_metrics, metrics_config, StatsConfig),
  application:ensure_all_started(oneup_metrics),


  StatsMap = oneup_metrics:init_from_config(StatsConfig),

  io:format("@@@@@@@ StatsMap: ~p~n", [StatsMap]),

  {oneup_counter, 'a.b.c1.d1.ref1', Ref1} = oneup_metrics:get_metric(StatsMap, [a, b, c1, d1, ref1]),
  {oneup_counter, 'a.b.c1.d2.ref2',Ref2} = oneup_metrics:get_metric(StatsMap, [a, b, c1, d2, ref2]),
  {oneup_counter, 'a.b.c2.d1.ref3', Ref3} = oneup_metrics:get_metric(StatsMap, [a, b, c2, d1, ref3]),
  {oneup_counter, 'a.b.c2.d1.ref4', Ref4} = oneup_metrics:get_metric(StatsMap, [a, b, c2, d1, ref4]),
  Ref1 = oneup_metric_config:get('a.b.c1.d1.ref1', oneup_counter),
  Ref2 = oneup_metric_config:get('a.b.c1.d1.ref2', oneup_counter),
  Ref3 = oneup_metric_config:get('a.b.c1.d1.ref3', oneup_counter),
  Ref4 = oneup_metric_config:get('a.b.c1.d1.ref4', oneup_counter),
  StatsA = oneup_metrics:get_sub_metrics(StatsMap, [a]),
  StatsAExpected = #{b => #{c1 => #{d1 => #{ref1 => {oneup_counter, 'a.b.c1.d1.ref1', Ref1}},
    d2 => #{ref2 => {oneup_counter, 'a.b.c1.d2.ref2', Ref2}}},
    c2 => #{d1 => #{ref3 => {oneup_counter, 'a.b.c2.d1.ref3', Ref3},
      ref4 => {oneup_counter, 'a.b.c2.d1.ref4', Ref4} }}}},

  StatsA = StatsAExpected,

  StatsB = oneup_metrics:get_sub_metrics(StatsMap, [a, b]),
  StatsBExpected = #{c1 => #{d1 => #{ref1 => {oneup_counter, 'a.b.c1.d1.ref1', Ref1}},
    d2 => #{ref2 => {oneup_counter, 'a.b.c1.d2.ref2', Ref2}}},
    c2 => #{d1 => #{ref3 => {oneup_counter, 'a.b.c2.d1.ref3', Ref3},
      ref4 => {oneup_counter, 'a.b.c2.d1.ref4', Ref4}}}},
  StatsB = StatsBExpected,

  StatsC1 = oneup_metrics:get_sub_metrics(StatsMap, [a, b, c1]),
  StatsC1Expected = #{d1 => #{ref1 => {oneup_counter, 'a.b.c1.d1.ref1', Ref1}}, d2 => #{ref2 => {oneup_counter, 'a.b.c1.d2.ref2', Ref2}}},
  StatsC1 = StatsC1Expected,

  StatsC2 = oneup_metrics:get_sub_metrics(StatsMap, [a, b, c2]),
  StatsC2Expected = #{d1 => #{ref3 => {oneup_counter, 'a.b.c2.d1.ref3', Ref3}, ref4 => {oneup_counter, 'a.b.c2.d1.ref4', Ref4} }},
  StatsC2 = StatsC2Expected,

  StatsC1D1 = oneup_metrics:get_sub_metrics(StatsMap, [a, b, c1, d1]),
  StatsC1D1Expected = #{ref1 => {oneup_counter, 'a.b.c1.d1.ref1', Ref1}},
  StatsC1D1 = StatsC1D1Expected,

  StatsC2D1 = oneup_metrics:get_sub_metrics(StatsMap, [a, b, c2, d1]),
  StatsC2D1Expected = #{ref3 => {oneup_counter, 'a.b.c2.d1.ref3', Ref3}, ref4 => {oneup_counter, 'a.b.c2.d1.ref4', Ref4}},
  StatsC2D1 = StatsC2D1Expected,

  Ref3Expected = oneup_metrics:get_metric(StatsMap, [a, b, c2, d1, ref3]),
  {oneup_counter, 'a.b.c2.d1.ref3', Ref3} = Ref3Expected,

  {oneup_counter, 'a2.b2.c2.d2.ref5', Ref5} = oneup_metrics:get_metric(StatsMap, [a2, b2, c2, d2, ref5]),
  0 = oneup:get(Ref5),

  StatsA2 = oneup_metrics:get_sub_metrics(StatsMap, [a2]),
  StatsA2Expected = #{b2 => #{c2 => #{d2 => #{ref5 => {oneup_counter, 'a2.b2.c2.d2.ref5', Ref5}}}}},
  StatsA2 = StatsA2Expected,

  application:stop(oneup_metrics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Counter test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO change to init_from_config
counter_test()->

  ct:print("Running counter_test()"),

  StatsConfig = [{oneup_counter,
    [ [a,b,c1,d1,ref1],
      [a,b,c1,d2,ref2],
      [a,b,c2,d1, ref3],
      [a, b, c2, d1, ref4]]}],

  application:ensure_all_started(lager),
  application:set_env(oneup_metrics, metrics_config, StatsConfig),
  application:ensure_all_started(oneup_metrics),


  StatsMap = oneup_metrics:init_from_config(StatsConfig),

  {oneup_counter, 'a.b.c1.d1.ref1', CounterRef1} = oneup_metrics:get_metric([a,b,c1,d1,ref1]),
  CounterRef1 = oneup_metric_config:get('a.b.c1.d1.ref1', oneup_counter),
  ct:print("CounterRef ~p for ~p in the map ~p", [CounterRef1, [a,b,c1,d1,ref1], StatsMap]),

  [oneup_metrics:update([a,b,c1,d1,ref1],oneup_counter, 2) || _I <- lists:seq(1,10)],

  20 = oneup_metrics:get('a.b.c1.d1.ref1',oneup_counter),

  [spawn(oneup_metrics, update, [[a,b,c1,d2,ref2],oneup_counter, 2]) || _I <- lists:seq(1,10)],
  timer:sleep(100),
  FinalCount = oneup_metrics:get([a,b,c1,d2,ref2],oneup_counter),
  io:format("Increment 10 times by 2 result: ~p~n", [FinalCount]),
  20 = FinalCount,

  [spawn(oneup_metrics, update, [[a,b,c1,d2,ref2]],oneup_counter) || _I <- lists:seq(1,10)],

  timer:sleep(100),
  30 = oneup_metrics:get('a.b.c1.d2.ref2',oneup_counter),

  [oneup_metrics:update([a,b,c2,d1, ref3], oneup_counter,N) || N <- lists:seq(1,10)],

  55 = oneup_metrics:get('a.b.c2.d1.ref3',oneup_counter),

  0 = oneup_metrics:get('a.b.c2.d1.ref4',oneup_counter),

  [spawn(oneup_metrics, update, [[a, b, c2, d1, ref4],oneup_counter, N]) || N <- lists:seq(1,10)],
  timer:sleep(100),

  55 = oneup_metrics:get('a.b.c2.d1.ref4',oneup_counter),

  oneup_metrics:reset('a.b.c1.d1.ref1',oneup_counter),
  0 = oneup_metrics:get('a.b.c1.d1.ref1',oneup_counter),

  30 = oneup_metrics:get('a.b.c1.d2.ref2',oneup_counter),
  application:stop(oneup_metrics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Counter test end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Gauge test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gauge_test()->
  ct:print("Running gauge_test()"),

  StatsConfig = [{oneup_gauge,
    [ [g,b,c1,d1,ref1],
      [g,b,c1,d2,ref2],
      [g,b,c2,d1, ref3],
      [g, b, c2, d1, ref4]]}],

  application:ensure_all_started(lager),
  application:set_env(oneup_metrics, metrics_config, StatsConfig),
  application:ensure_all_started(oneup_metrics),

  StatsMap = oneup_metrics:init_from_config(StatsConfig),

  %% TODO temp check to verify updated oneup lib
  C = oneup:new_counter(),
  0 = oneup:set(C, 123),
  123 = oneup:set(C, 10),

  0 = oneup_metrics:update([g,b,c1,d1,ref1], oneup_gauge, 123),
  123 = oneup_metrics:get('g.b.c1.d1.ref1',oneup_gauge),

  [N = oneup_metrics:update([g,b,c2,d1,ref3],oneup_gauge, N) + 1 || N <- lists:seq(1,10)],

  10 = oneup_metrics:get('g.b.c2.d1.ref3',oneup_gauge),

  [spawn(oneup_metrics, update, [[g, b, c2, d1, ref4],oneup_gauge, N]) || N <- lists:seq(1,10)],
  timer:sleep(100),
  Ref4Value = oneup_metrics:get([g, b, c2, d1, ref4],oneup_gauge),

  case Ref4Value of
    N when N < 1 -> true = false;
    N when N > 10 -> true = false;
    N -> ok
  end,
  application:stop(oneup_metrics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Gauge test end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Meter test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO change to init_from_config;
testing_meter() ->
  ct:print("Running meter_test()"),
  StatsConfig = [{oneup_meter,
    [ [g,b,c1,d1,ref1],
      [g,b,c1,d2,ref2]]}],
  application:ensure_all_started(lager),
  application:set_env(oneup_metrics, metrics_config, StatsConfig),
  application:ensure_all_started(oneup_metrics),


  StatsMap = oneup_metrics:init_from_config(StatsConfig),


  [0, Mean, 0, 0, 0, 0, 0, 0] = oneup_metrics:get([g,b,c1,d1,ref1],oneup_meter),
  %ct:print("MEAN PROBLEM! ~p~n~n", [[Counter, Mean, InstantRate, OneMinRate, FiveMinRate, FifteenMinRate, HourRate, DayRate]]),
  %0 = Counter,
  ?assert(0 == Mean),
  %0 = InstantRate,
  %0 = OneMinRate,
  %0 = FifteenMinRate,
  %0 = FiveMinRate,
  %0 = HourRate,
  %0 = DayRate,
  oneup_metrics:update([g,b,c1,d1,ref1],oneup_meter, 1000),
  [Counter, _, InstantRate, OneMinRate, FiveMinRate, FifteenMinRate, HourRate, DayRate] = oneup_metrics:get([g,b,c1,d1,ref1],oneup_meter),
  1000 = Counter ,
  0 = InstantRate,
  0 = OneMinRate,
  0 =FiveMinRate,
  0 =FifteenMinRate,
  0 =HourRate,
  0 =DayRate,

  timer:sleep(5000),
  [Counter_new, Mean_new, InstantRate_new, OneMinRate_new, FiveMinRate_new, FifteenMinRate_new, HourRate_new, DayRate_new] = oneup_metrics:get([g,b,c1,d1,ref1],oneup_meter),
  0 = Counter_new,
  ?assert(Mean_new < 300),
  200 = InstantRate_new,
  OneMinRate_new = tick(1,1000,0),
  FiveMinRate_new = tick(5,1000,0),
  FifteenMinRate_new = tick(15,1000,0),
  HourRate_new = tick(60, 1000, 0),
  DayRate_new = tick(1440, 1000, 0),
  [0, 0.0, 0, 0, 0, 0, 0, 0] = oneup_metrics:get([g,b,c1,d2,ref2],oneup_meter),





  timer:sleep(60000),
  [Counter_rst, _, InstantRate_rst, OneMinRate_rst, FiveMinRate_rst, _, _, _] = oneup_metrics:get([g,b,c1,d1,ref1],oneup_meter),
  0 = Counter_rst,
  0 =InstantRate_rst,
  true = OneMinRate_rst < 1,
  true = FiveMinRate_rst > 2.6,
  true = FiveMinRate_rst < 2.8,
  application:stop(oneup_metrics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Meter test end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Histogram test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

histogram_test()->
  ct:print("Running histogram_test()"),
  StatsConfig = [{oneup_histogram,
    [ [g,b,c1,d1,ref1],
      [g,b,c1,d2,ref2]]}],
  application:ensure_all_started(lager),
  application:set_env(oneup_metrics, metrics_config, StatsConfig),
  application:ensure_all_started(oneup_metrics),

  StatsMap = oneup_metrics:init_from_config(StatsConfig),
  {0, 0, 0, 0} = oneup_metrics:get([g,b,c1,d1,ref1],oneup_histogram),

  oneup_metrics:update([g,b,c2,d1,ref3],oneup_histogram,10),
  {1, 10, 10, 10} = oneup_metrics:get([g,b,c1,d1,ref1],oneup_histogram),

  oneup_metrics:update([g,b,c2,d1,ref3],oneup_histogram,20),
  {2, 15, 10, 20} = oneup_metrics:get([g,b,c1,d1,ref1],oneup_histogram),
  {0, 0, 0, 0} = oneup_metrics:get([g,b,c1,d2,ref2],oneup_histogram),
  %{oneup_histogram, 'a.b.c1.d1.ref1', Val_ref, Sample_ref, Min_ref, Max_ref} = oneup_metrics:get_metric(StatsMap, [a, b, c1, d1, ref1]),
  timer:sleep(60000),
  oneup_metrics:update([g,b,c2,d1,ref3],oneup_histogram,35),
  {2, 25, 10, 35} = oneup_metrics:get([g,b,c1,d1,ref1],oneup_histogram),
  oneup_metrics:reset([g,b,c1,d1,ref1],oneup_histogram),
  {0, 0, 999999999999, 0} = oneup_metrics:get([g,b,c1,d1,ref1],oneup_histogram),
  application:stop(oneup_metrics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Histogram test end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


direct_inc_sequential_test()->
  CounterRef = oneup:new_counter(),

  Samples = 1000000,
  TotalTime = lists:foldl(
    fun(_X, Total)->
      {Time, _Result} = timer:tc(oneup, inc, [CounterRef]),
      Total + Time
    end, 0, lists:seq(1, Samples)),

  Samples = oneup:get(CounterRef),
  %% This is super fast when sequential, so perfect for the tcp receiver loop
  verify_avg_time(TotalTime, Samples, 0.3),
  application:stop(oneup_metrics).

direct_inc_parallel_test()->
  CounterRef = oneup:new_counter(),
  TotalTimeAccRef = oneup:new_counter(),
  Samples = 100000,
  Fun = fun() -> timer:sleep(1000), {Time, _Res} = timer:tc(oneup, inc, [CounterRef]), oneup:inc2(TotalTimeAccRef, Time) end,
  [spawn(Fun) || _X <- lists:seq(1, Samples)],

  timer:sleep(2000),

  Samples = oneup:get(CounterRef),
  TotalTime = oneup:get(TotalTimeAccRef),

  %% trying to access the counter ref by multiple processes simultaneously is obviously slower than doing it sequentially
  verify_avg_time(TotalTime, Samples, 2.5).

%%perf_depth5_test()->
%%
%%  StatsConfig = [
%%    {oneup_counter, [[a,b,c1,d1,ref1],
%%    [a,b,c1,d2,ref2],
%%    [a,b,c2,d1, ref3],
%%    [a, b, c2, d1, ref4],
%%    [a2, b2, c2, d2, ref5],
%%    [a2, b3, c3, d2, ref6],
%%    [a2, b3, c3, d3, ref7],
%%    [a3, b1, c1, d1, ref8],
%%    [a3, b1, c2, d2, ref9],
%%    [a3, b2, c3, d10, ref10]]}],
%%
%%  application:ensure_all_started(lager),
%%  application:set_env(oneup_metrics, metrics_config, StatsConfig),
%%  application:ensure_all_started(oneup_metrics),
%%
%%
%%  StatsMap = oneup_metrics:init_from_config(StatsConfig),
%%
%%  {Total, Samples} = lists:foldl(
%%    fun(X, {Total, Samples} = Acc)->
%%      {Time, _Result} = timer:tc(oneup_metrics, update_metric, [StatsMap, X]),
%%      {Total + Time, Samples + 1}
%%    end, {0,0}, [lists:nth(I rem 5 + 1, proplists:get_value(oneup_counter, StatsConfig)) || I <- lists:seq(1, 100000)]),
%%
%%  verify_avg_time(Total, Samples, 5),
%%  application:stop(oneup_metrics).
%%
%%perf_depth7_test() ->
%%  ConfigMetrics = [[a, b, c1, d1, e1, f1, ref1],
%%    [a, b, c1, d2, e1, f1, ref2],
%%    [a, b, c2, d1, e1, f2, ref3],
%%    [a, b, c2, d1, e1, f2, ref4],
%%    [a2, b2, c2, d2, e1, f2, ref5],
%%    [a2, b3, c3, d2, e1, f2, ref6],
%%    [a2, b3, c3, d3, e1, f2, ref7],
%%    [a3, b1, c1, d1, e1, f2, ref8],
%%    [a3, b1, c2, d2, e1, f2, ref9],
%%    [a3, b2, c3, d10, e1, f2, ref10]],
%%
%%  StatsConfig = [{oneup_counter,
%%    ConfigMetrics
%%  }],
%%
%%  application:ensure_all_started(lager),
%%  application:set_env(oneup_metrics, metrics_config, StatsConfig),
%%  application:ensure_all_started(oneup_metrics),
%%
%%
%%  StatsMap = oneup_metrics:init_from_config(StatsConfig),
%%  oneup_metrics:enable(StatsMap),
%%
%%  {Total1, Samples1} = lists:foldl(
%%    fun(X, {Total, Samples} = Acc)->
%%      {Time, _Result} = timer:tc(oneup_metrics, update_metric, [StatsMap, X]),
%%      {Total + Time, Samples + 1}
%%    end, {0,0}, [lists:nth(I rem 7 + 1, ConfigMetrics) || I <- lists:seq(1, 100000)]),
%%
%%  verify_avg_time(Total1, Samples1, 6),
%%
%%  {Total2, Samples2} = lists:foldl(
%%    fun(X, {Total, Samples} = Acc)->
%%      {Time, _Result} = timer:tc(oneup_metrics, update_metric, [StatsMap, X, 999]),
%%      {Total + Time, Samples + 1}
%%    end, {0,0}, [lists:nth(I rem 7 + 1, ConfigMetrics) || I <- lists:seq(1, 100000)]),
%%
%%  verify_avg_time(Total2, Samples2, 6),
%%
%%  {Total3, Samples3} = lists:foldl(
%%    fun(Element, {Total, Samples} = Acc)->
%%      Value = rand:uniform(10000000000),
%%      {Time, _Result} = timer:tc(oneup_metrics, update_metric, [StatsMap, Element, Value]),
%%      {Total + Time, Samples + 1}
%%    end, {0,0}, [lists:nth(I rem 7 + 1, ConfigMetrics) || I <- lists:seq(1, 100000)]),
%%
%%  verify_avg_time(Total3, Samples3, 6),
%%  application:stop(oneup_metrics).

verify_avg_time(Total, Samples, Micros) ->
  AvgTime = Total/Samples,
  ct:print("AvgTime ~p", [AvgTime]),
  ?assert(AvgTime < Micros).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%helper functions for meter and histogram
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
alpha(Minutes)->
  1 - math:exp(-math:pow(?INTERVAL,2) / ?SECONDS_PER_MINUTE / math:pow(Minutes,2)).

tick(_Minutes, Count, undefined)->
  Count / ?INTERVAL;  %% just return instant rate
tick(Minutes, Count, PrevRate)->
  InstantRate = Count / ?INTERVAL,
  PrevRate + (alpha(Minutes) * (InstantRate - PrevRate)).