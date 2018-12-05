%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2017 12:28 PM
%%%-------------------------------------------------------------------
-module(oneup_metrics).
-author("iguberman").

-compile(export_all).

-define(METRICS_MAP, metrics_config).

-define(METRIC_TYPES, [counter, gauge, meter, histogram]).

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  get_value/1,
  reset/1,
  reset/2,
  display/1,
  display/2,
  get_sub_metrics/2,
  evaluated_metrics/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {metrics}).


-type counters() :: reference() | [reference(), ... ].
-type metric_module() :: atom().
-type metric_gen_name() :: atom().
-type metric() :: {metric_module(), metric_gen_name(), counters()}.
-type metric_name() :: [atom(), ...].

%% return one or more ref counters with implementing module name,
%% i.e. histogram will have two ref counters per one metric, so returns
%% { oneup_histogram, ValueAggregateCounterReference, OccurrenceCounterReference}
%% while a counter will return:
%% { oneup_counter, ValueCounterReference}

-callback init_metric(MetricName :: metric_name()) -> Response :: metric().

%% NOTE: updates are the two callbacks that are expected to be called very frequently and
%% therefore we pass the counter refs directly from the calling process
%% which is supposed to keep around the metrics map with all the atomic counter refs to be able to do that without
%% requesting this info from some central place thus avoiding any bottlenecks.

%% update with specific value
-callback update(Counters :: counters(), Value :: number()) -> Response :: any().

%% update with one
-callback update(Counters :: counters()) -> Response :: any().

-callback header()-> Response :: string().

%%%===================================================================
%%% API
%%%===================================================================

%%% This method is to retrieve metrics map for the first time for top level processes or supervisors
%%% to pass on to their children
initial_get_config()->
  {ok, Metrics} = gen_server:call(?SERVER, metrics),
  Metrics.

%%% This method is to retrieve partial metrics map for top level processes to pass on to their children
%%% It is highly advisable for performance and memory considrations to only pass the portion of Metrics
%%% that will potentially be updated by the child process
initial_get_config(Prefix)->
  {ok, Metrics} = gen_server:call(?SERVER, {metrics, Prefix}),
  Metrics.

enable(MetricsMap)->
  put(?METRICS_MAP, MetricsMap).

enable(Prefix, MetricsMap)->
  put(?METRICS_MAP, get_sub_metrics(MetricsMap, Prefix)).

add_multiple(NewMetrics)->
  gen_server:call(?SERVER, {add_multiple, NewMetrics}).

add(NewMetric) ->
  gen_server:call(?SERVER, {add, NewMetric}).


start_link(MetricsMap) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [MetricsMap], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MetricsMap]) ->
  {ok, #state{metrics = MetricsMap}}.

handle_call(metrics, _From, #state{metrics = Metrics} = State) ->
  {reply, {ok, Metrics}, State};
handle_call({metrics, Prefix}, _From, #state{metrics = Metrics} = State) when is_list(Prefix) ->
  SubMetrics = get_sub_metrics(Metrics, Prefix),
  {reply, {ok, SubMetrics}, State};
handle_call({add, Metric}, _From, #state{ metrics = Metrics } = State)->
  ExpandedMetrics = add_metric(Metric, Metrics),
  {reply, {ok, ExpandedMetrics}, State#state{metrics = ExpandedMetrics}};
handle_call({add_multiple, NewMetrics}, _From, #state{ metrics = Metrics } = State)->
  ExpandedMetrics = add_metrics(NewMetrics, Metrics),
  {reply, {ok, ExpandedMetrics}, State#state{metrics = ExpandedMetrics}}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Utilities
%%%===================================================================


metric_name_to_atom(MetricName)->
  list_to_atom(string:join([maybe_atom_to_list(Part) || Part <- MetricName], ".")).


maybe_atom_to_list(Part) when is_binary(Part)-> maybe_atom_to_list(binary_to_list(Part));
maybe_atom_to_list(Part) when is_list(Part)-> Part;
maybe_atom_to_list(Part) when is_atom(Part)-> atom_to_list(Part).


%%% Metrics Map is expected to be in the process dictionary of any process that got it
%%% from its supervisor or parent process
%%% All the update methods in this module depend on this being the case
%%% However, update_metrics methods don't -- they just take a MetricsMap as first argument
%%% no need to worry about bottlenecks
config(MetricsMapKey) ->
  case erlang:get(MetricsMapKey) of
    undefined -> lager:warning("Metrics not enabled in process ~p ~p", [self(), process_info(self(), registered_name)]);
    MetricsMap -> MetricsMap
  end.

config()->
  config(?METRICS_MAP).

init_from_config(Config) ->
  init_from_config([global], Config).

init_from_config(Domain, Config) when is_list(Config), is_list(Domain) ->
  lists:foldl(fun(Type, AllMetrics) -> add_metrics(Domain, {Type, proplists:get_value(Type, Config)}, AllMetrics) end, #{}, proplists:get_keys(Config)).


add_metrics(Domain, {Type, NewMetrics}, InitialMetrics) ->
  SpecificTypeMetrics = lists:foldl(fun(Metric, AllMetrics) -> add_metric({Type, Domain ++ Metric}, AllMetrics) end, InitialMetrics, NewMetrics),
  lager:info("~p metrics initialized to ~p", [Type, SpecificTypeMetrics]),
  SpecificTypeMetrics.

add_metric({Type, Metric}, AllMetrics) when is_list(Metric)->
  add_nested_metric(AllMetrics, Metric, Metric, Type).

%% Please note that it would've been more elegant to pattern-match
%% in function args instead of case metric elements but it doesn't work with maps
%% due to non-guaranteed order of argument resolution

%% Reached the end of metric-name list, initialize the metric
add_nested_metric(Metrics, Metric, [Last], Type) when is_map(Metrics), is_atom(Type)->
  case Metrics of
    #{Last := {Type, _MetricName, _ExistingCounter} } when is_atom(Type) ->
      lager:error("Duplicate entry in metrics config: ~p!  Exiting...", [Metric]),
      true = false;
    _ ->
      lager:info("Adding metric ~p", [Metric]),
      Metrics#{Last => Type:init_metric(Metric) }
  end;
add_nested_metric(Metrics, Metric, [Head | Tail], Type) when is_map(Metrics)->
  case Metrics of
    %% Existing entry
    #{ Head := NestedMetrics} -> Metrics#{Head => add_nested_metric(NestedMetrics, Metric, Tail, Type)};
    %% First encounter of this entry
    _ -> Metrics#{Head => add_nested_metric(#{}, Metric, Tail, Type)}
  end.



get_value(Domain, MetricName) when is_atom(Domain) ->
  get_value([Domain] ++ MetricName);
get_value(Domain, MetricName) when is_list(Domain)->
  get_value(Domain ++ MetricName).

get_value(MetricName) when is_list(MetricName)->
  get_value(metric_name_to_atom(MetricName));
get_value(MetricName) when is_atom(MetricName)->
  gen_server:call(MetricName, get).


reset(Domain, MetricName) when is_atom(Domain)->
  reset([Domain] ++ MetricName);
reset(Domain, MetricName) when is_list(Domain)->
  reset(Domain ++ MetricName).

reset(MetricName) when is_list(MetricName)->
  reset(metric_name_to_atom(MetricName));
reset(MetricName) when is_atom(MetricName)->
  gen_server:call(MetricName, reset).

display(MetricName)->
  display([], MetricName).

display(Domain, MetricName) when is_atom(Domain) ->
  display([Domain], MetricName);
display(Domain, MetricName) when is_list(Domain), is_list(MetricName)->
  display(Domain, metric_name_to_atom(Domain ++ MetricName));
display(Domain, MetricName) when is_atom(MetricName)->
  gen_server:call(MetricName, {display, Domain}).

update({Type, CounterRef}) when is_atom(Type)->
  Type:update(CounterRef);
update(MetricName) when is_list(MetricName)->
  case get_metric(MetricName) of
    {error, Error} -> lager:error("Error getting metric ~p: ~p", [MetricName, Error]);
    {Type, _MetricName, CounterRefs} -> update({Type, CounterRefs})
  end;
update(Unexpected)->
  lager:error("Unexpected arg in update: ~p", [Unexpected]).

update({MetricType, CounterRef}, Value) when is_atom(MetricType)->
  MetricType:update(CounterRef, Value);
update(MetricName, Value) when is_list(MetricName)->
  case get_metric(MetricName) of
    {error, Error} -> lager:error("Error getting metric ~p: ~p", [MetricName, Error]);
    {Type, _MetricName, CounterRefs} -> update({Type, CounterRefs}, Value)
  end.


update_metric(MetricsMap, MetricName) when is_map(MetricsMap) ->
  case get_metric(MetricsMap, MetricName) of
    {error, uninitialized} -> lager:warning("Requesting uninitialized metric ~p", [MetricName]);
    {Type, _MetricName, Counters} -> update({Type, Counters})
  end.

update_metric(MetricsMap, MetricName, Value) when is_map(MetricsMap)->
  case get_metric(MetricsMap, MetricName) of
    {error, uninitialized} -> lager:warning("Requesting uninitialized metric ~p", [MetricName]);
    {Type, _MetricName, Counters} -> update({Type, Counters}, Value)
  end.

get_metric(Metric)->
  get_metric(config(), Metric).

get_metric(Metrics, [Metric]) ->
  case Metrics of
    #{Metric := {MetricType, MetricName, CounterRef}} when is_atom(MetricType) -> {MetricType, MetricName, CounterRef};
    _-> {error, uninitialized}
  end;
get_metric(Metrics, [Head | Tail])->
  case Metrics of
    #{Head := NestedMetric} -> get_metric(NestedMetric, Tail);
    _-> {error, uninitialized}
  end.

get_metric_values(Metric)->
  get_metric_values(config(), Metric).

get_metric_values(Metrics, [Metric]) ->
  case Metrics of
    #{Metric := {_MetricType, MetricName, _CounterRef}} -> oneup_metrics:get_value(MetricName);
    _-> {error, uninitialized}
  end;
get_metric_values(Metrics, [Head | Tail])->
  case Metrics of
    #{Head := NestedMetric} -> get_metric_values(NestedMetric, Tail);
    _-> {error, uninitialized}
  end.

get_sub_metrics(Metrics, [Metric]) ->
  case Metrics of
    #{Metric := ExpectedMetric} -> ExpectedMetric;
    _-> {error, uninitialized}
  end;
get_sub_metrics(Metrics, [Head | Tail])->
  case Metrics of
    #{Head := NestedMetric} -> get_sub_metrics(NestedMetric, Tail);
    _-> {error, uninitialized}
  end.

reset_counters(MetricsMap) ->
  maps:fold(fun(_Key, Val, _Acc) -> reset_counter(Val) end, 'N/A', MetricsMap).

reset_counter({Type, MetricName, _CounterRef}) when is_atom(Type) ->
  reset(MetricName);
reset_counter(Val) when is_map(Val)->
  reset_counters(Val).

current_second() ->
  {Mega, Sec, _Micro} = os:timestamp(),
  (Mega * 1000000 + Sec).

evaluated_metrics(MetricsMap) ->
  MetricsMap.

%% TODO CREATE A NEW MAP from original metrics, the one with all ref counters evaluated.
%%evaluate_metrics(MetricsMap) when is_map(MetricsMap)->
%%  evaluate_metrics(MetricsMap, maps:new()).
%%
%%evaluate_metrics(OriginalMetricsMap, EvaluatedMetricsMap) ->
%%  maps:fold(fun(Key, Val, Acc) -> evaluate_metric(Val, Acc) end, EvaluatedMetricsMap, OriginalMetricsMap).
%%
%%evaluate_metric(Key, {_MetricType, MetricName, _Counters}, EvaluatedMetricsMap)  ->
%%  UpdatedMap = maps:update(MetricName, oneup_metrics:get_metric_values(MetricName))
%%  Body ++ oneup_metrics:display(MetricName);
%%evaluate_metric(Val, Body) when is_map(Val)->
%%  evaluate_metrics(Val, Body).