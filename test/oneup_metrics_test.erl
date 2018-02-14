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

%%@@@@@@@@@@@@@@@@@@@@@@@@@@@  EUNIT @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

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


  StatsMap = oneup_metrics:initial_get_config(),
  oneup_metrics:enable(StatsMap),

  {oneup_counter, 'a.b.c1.d1.ref1', CounterRef1} = oneup_metrics:get_metric([a,b,c1,d1,ref1]),

  ct:print("CounterRef ~p for ~p in the map ~p", [CounterRef1, [a,b,c1,d1,ref1], StatsMap]),

  [oneup_metrics:update({oneup_counter, CounterRef1}, 2) || _I <- lists:seq(1,10)],

  20 = oneup_metrics:get_value('a.b.c1.d1.ref1'),

  [spawn(oneup_metrics, update_metric, [StatsMap, [a,b,c1,d2,ref2], 2]) || _I <- lists:seq(1,10)],
  timer:sleep(100),
  FinalCount = oneup_metrics:get_value([a,b,c1,d2,ref2]),
  io:format("Increment 10 times by 2 result: ~p~n", [FinalCount]),
  20 = FinalCount,

  [spawn(oneup_metrics, update_metric, [StatsMap, [a,b,c1,d2,ref2]]) || _I <- lists:seq(1,10)],

  timer:sleep(100),
  30 = oneup_metrics:get_value('a.b.c1.d2.ref2'),

  [oneup_metrics:update_metric(StatsMap, [a,b,c2,d1, ref3], N) || N <- lists:seq(1,10)],

  55 = oneup_metrics:get_value('a.b.c2.d1.ref3'),

  0 = oneup_metrics:get_value('a.b.c2.d1.ref4'),

  [spawn(oneup_metrics, update_metric, [StatsMap, [a, b, c2, d1, ref4], N]) || N <- lists:seq(1,10)],
  timer:sleep(100),

  55 = oneup_metrics:get_value('a.b.c2.d1.ref4'),

  oneup_metrics:reset('a.b.c1.d1.ref1'),
  0 = oneup_metrics:get_value('a.b.c1.d1.ref1'),

  30 = oneup_metrics:get_value('a.b.c1.d2.ref2'),
  application:stop(oneup_metrics).

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

  StatsMap = oneup_metrics:initial_get_config(),
  oneup_metrics:enable(StatsMap),

  %% TODO temp check to verify updated oneup lib
  C = oneup:new_counter(),
  0 = oneup:set(C, 123),
  123 = oneup:set(C, 10),

  0 = oneup_metrics:update_metric(StatsMap, [g,b,c1,d1,ref1], 123),
  123 = oneup_metrics:get_value('g.b.c1.d1.ref1'),

  [N = oneup_metrics:update_metric(StatsMap, [g,b,c2,d1,ref3], N) + 1 || N <- lists:seq(1,10)],

  10 = oneup_metrics:get_value('g.b.c2.d1.ref3'),

  [spawn(oneup_metrics, update_metric, [StatsMap, [g, b, c2, d1, ref4], N]) || N <- lists:seq(1,10)],
  timer:sleep(100),
  Ref4Value = oneup_metrics:get_value([g, b, c2, d1, ref4]),

  case Ref4Value of
    N when N < 1 -> true = false;
    N when N > 10 -> true = false;
    N -> ok
  end,
  application:stop(oneup_metrics).


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

perf_depth5_test()->

  StatsConfig = [
    {oneup_counter, [[a,b,c1,d1,ref1],
    [a,b,c1,d2,ref2],
    [a,b,c2,d1, ref3],
    [a, b, c2, d1, ref4],
    [a2, b2, c2, d2, ref5],
    [a2, b3, c3, d2, ref6],
    [a2, b3, c3, d3, ref7],
    [a3, b1, c1, d1, ref8],
    [a3, b1, c2, d2, ref9],
    [a3, b2, c3, d10, ref10]]}],

  application:ensure_all_started(lager),
  application:set_env(oneup_metrics, metrics_config, StatsConfig),
  application:ensure_all_started(oneup_metrics),


  StatsMap = oneup_metrics:init_from_config(StatsConfig),

  {Total, Samples} = lists:foldl(
    fun(X, {Total, Samples} = Acc)->
      {Time, _Result} = timer:tc(oneup_metrics, update_metric, [StatsMap, X]),
      {Total + Time, Samples + 1}
    end, {0,0}, [lists:nth(I rem 5 + 1, proplists:get_value(oneup_counter, StatsConfig)) || I <- lists:seq(1, 100000)]),

  verify_avg_time(Total, Samples, 5),
  application:stop(oneup_metrics).

perf_depth7_test() ->
  ConfigMetrics = [[a, b, c1, d1, e1, f1, ref1],
    [a, b, c1, d2, e1, f1, ref2],
    [a, b, c2, d1, e1, f2, ref3],
    [a, b, c2, d1, e1, f2, ref4],
    [a2, b2, c2, d2, e1, f2, ref5],
    [a2, b3, c3, d2, e1, f2, ref6],
    [a2, b3, c3, d3, e1, f2, ref7],
    [a3, b1, c1, d1, e1, f2, ref8],
    [a3, b1, c2, d2, e1, f2, ref9],
    [a3, b2, c3, d10, e1, f2, ref10]],

  StatsConfig = [{oneup_counter,
    ConfigMetrics
  }],

  application:ensure_all_started(lager),
  application:set_env(oneup_metrics, metrics_config, StatsConfig),
  application:ensure_all_started(oneup_metrics),


  StatsMap = oneup_metrics:init_from_config(StatsConfig),
  oneup_metrics:enable(StatsMap),

  {Total1, Samples1} = lists:foldl(
    fun(X, {Total, Samples} = Acc)->
      {Time, _Result} = timer:tc(oneup_metrics, update_metric, [StatsMap, X]),
      {Total + Time, Samples + 1}
    end, {0,0}, [lists:nth(I rem 7 + 1, ConfigMetrics) || I <- lists:seq(1, 100000)]),

  verify_avg_time(Total1, Samples1, 6),

  {Total2, Samples2} = lists:foldl(
    fun(X, {Total, Samples} = Acc)->
      {Time, _Result} = timer:tc(oneup_metrics, update_metric, [StatsMap, X, 999]),
      {Total + Time, Samples + 1}
    end, {0,0}, [lists:nth(I rem 7 + 1, ConfigMetrics) || I <- lists:seq(1, 100000)]),

  verify_avg_time(Total2, Samples2, 6),

  {Total3, Samples3} = lists:foldl(
    fun(Element, {Total, Samples} = Acc)->
      Value = rand:uniform(10000000000),
      {Time, _Result} = timer:tc(oneup_metrics, update_metric, [StatsMap, Element, Value]),
      {Total + Time, Samples + 1}
    end, {0,0}, [lists:nth(I rem 7 + 1, ConfigMetrics) || I <- lists:seq(1, 100000)]),

  verify_avg_time(Total3, Samples3, 6),
  application:stop(oneup_metrics).

verify_avg_time(Total, Samples, Micros) ->
  AvgTime = Total/Samples,
  ct:print("AvgTime ~p", [AvgTime]),
  ?assert(AvgTime < Micros).
