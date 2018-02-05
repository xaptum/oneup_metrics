%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2017 3:16 PM
%%%-------------------------------------------------------------------
-module(functional_SUITE).
-author("iguberman").

-define(TEST_CONFIG, [
  {oneup_counter, [[a, b, c1, d1, ref1],
  [a, b, c1, d2, ref2],
  [a, b, c2, d1, ref3],
  [a, b, c2, d1, ref4],
  [a2, b2, c2, d2, ref5]]}
]).

-include_lib("common_test/include/ct.hrl").

%% API
-export([
  init_per_group/2,
  end_per_group/2,
  init_per_suite/1,
  end_per_suite/1,
  all/0]).

-define(DEVICES_PER_NODE, 500).
-define(SUBS_PER_NODE, 1).
-define(MESSAGES_PER_DEVICE, 10).

%% API
-export([test_metrics_init/1,
  test_http_reporter/1,
  test_system_info_reporter/1,
  test_metric_updates/1,
  test_metric_add/1,
  test_metric_add_multiple/1]).

all() -> [
  test_metrics_init,
  test_http_reporter,
  test_system_info_reporter,
  test_metric_updates,
  test_metric_add,
  test_metric_add_multiple].

init_per_suite(Config) ->
  application:ensure_all_started(lager),
  application:ensure_all_started(oneup_metrics),
  ct:print("loaded apps: ~p", [application:loaded_applications()]),
  true = lists:member({cowboy,"Small, fast, modern HTTP server.","2.2.0"}, application:loaded_applications()),
  Config.

init_per_group(_, Config) ->
  Config.

end_per_group(_, _Config) ->
  ok.

end_per_suite(Config) ->
 Config.

test_metrics_init(Config) ->
  {ok, MetricsConfig} = application:get_env(oneup_metrics, metrics_config),
  ExpectedMetricsMap = oneup_metrics:init_from_config(MetricsConfig),
  InitializedMetricsMap = oneup_metrics:initial_get_config(),
  deep_compare(ExpectedMetricsMap, InitializedMetricsMap),
 Config.

test_http_reporter(Config)->
  {ok, HttpPort} = application:get_env(oneup_metrics, http_port),
  CurlResult = os:cmd("curl -s localhost:" ++ integer_to_list(HttpPort) ++ "/a"),
  ct:print("CURL RESULT:~n~s", [CurlResult]),
  ExpectedResult = lists:flatten(io_lib:format("b.c1.d1.ref1: 0~nb.c1.d2.ref2: 0~nb.c2.d1.ref3: 0~nb.c2.d1.ref4: 0", [])),
  ct:print("Excpected Result:~n~s", [ExpectedResult]),
%%  true = string:equal(CurlResult, ExpectedResult),
  Config.

test_system_info_reporter(Config)->
  {ok, HttpPort} = application:get_env(oneup_metrics, http_port),
  CurlResult = os:cmd("curl -s localhost:" ++ integer_to_list(HttpPort) ++ "/system_info"),
  ct:print("CURL RESULT:~n~s", [CurlResult]),
  Config.

test_metric_updates(Config)->
  InitializedMetricsMap = oneup_metrics:initial_get_config(),
  oneup_metrics:enable(InitializedMetricsMap),
  [oneup_metrics:increment(Metric) || Metric <- ?TEST_CONFIG],
  [1 = oneup_metrics:get(Metric) || Metric <- ?TEST_CONFIG],
  [oneup_metrics:increment(Metric) || Metric <- ?TEST_CONFIG],
  [2 = oneup_metrics:get(Metric) || Metric <- ?TEST_CONFIG],
  [oneup_metrics:increment(Metric, 2) || Metric <- ?TEST_CONFIG],
  [4 = oneup_metrics:get(Metric) || Metric <- ?TEST_CONFIG],
  [oneup_metrics:reset(Metric) || Metric <- ?TEST_CONFIG],
  [0 = oneup_metrics:get(Metric) || Metric <- ?TEST_CONFIG],
  [oneup_metrics:set(Metric, 10) || Metric <- ?TEST_CONFIG],
  [10 = oneup_metrics:get(Metric) || Metric <- ?TEST_CONFIG],
  Config.

test_metric_add(Config)->
  NewMetric = [x,y,z],
  oneup_metrics:add_metric({oneup_counter, NewMetric}),
  ModifiedMetricsMap = oneup_metrics:initial_get_config(),
  ct:print("ModifiedMetricsMap: ~p", [ModifiedMetricsMap]),
  #{x := #{y := #{z := NewCounter}}} = ModifiedMetricsMap,
  0 = oneup:get(NewCounter),
  oneup_metrics:enable(ModifiedMetricsMap),
  oneup_metrics:increment(NewMetric),
  1 = oneup_metrics:get(NewMetric),
  Config.

test_metric_add_multiple(Config)->
  NewMetrics = [[x,y,z1],[x,y,z2],[k,l,m]],
  oneup_metrics:add_multiple(NewMetrics),
  MultiAddedMetricsMap = oneup_metrics:initial_get_config(),
  ct:print("MultiAddedMetricsMap ~p", [MultiAddedMetricsMap]),
  oneup_metrics:enable(MultiAddedMetricsMap),
  [oneup_metrics:increment(Metric, 1000000) || Metric <- NewMetrics],
  [1000000 = oneup_metrics:get(Metric) || Metric <- NewMetrics],

  {ok, HttpPort} = application:get_env(oneup_metrics, http_port),

  CurlResult = os:cmd("curl -s localhost:" ++ integer_to_list(HttpPort)),
  ct:print("CURL RESULT:~n~s", [CurlResult]),

  CurlResult_X = os:cmd("curl -s localhost:" ++ integer_to_list(HttpPort) ++ "/x"),
  ct:print("X CURL RESULT:~n~s", [CurlResult_X]),


  Config.


deep_compare(MetricsMap1, MetricsMap2)->
  ct:print("~p", [MetricsMap1]),
  ct:print("~p", [MetricsMap2]),
  ok.


