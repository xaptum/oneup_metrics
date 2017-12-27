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

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {metrics}).

%%%===================================================================
%%% API
%%%===================================================================

%%% This method is to retrieve metrics map for the first time for top level processes or supervisors
%%% to pass on to ther children
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
  put(?METRICS_MAP, get_metrics(MetricsMap, Prefix)).

%%TODO reset()->
%%  ok = gen_server:call(?SERVER, reset).

add_multiple(NewMetrics)->
  {ok, _ExpandedMetrics} = gen_server:call(?SERVER, {add_multiple, NewMetrics}).

add(NewMetric) ->
  {ok, _ExpandedMetrics} = gen_server:call(?SERVER, {add, NewMetric}).


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
  SubMetrics = get_metrics(Metrics, Prefix),
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

%%% Metrics Map is expected to be in the process dictionary of any process that got it
%%% from its supervisor or parent process
%%% All the update methods in this module depend on this being the case
%%% If this sounds like a blasphemous Erlang antipattern to you, please recall this
%%% library is based on global counters which are an Erlang blasphemy to begin with ;)
%%% no need to worry about bottlenecks
config()->
  erlang:get(?METRICS_MAP).


%%% TODO only counters/gauges are supported right now
%%% Histograms and meters coming soon
init_from_config(Config) when is_list(Config)->
  add_metrics(Config, #{}).

add_metrics(NewMetrics, InitialMetrics)->
  Metrics = lists:foldl(fun(Metric, AllMetrics) -> add_metric(Metric, AllMetrics) end, InitialMetrics, NewMetrics),
  lager:info("Metrics initialized to ~p", [Metrics]),
  Metrics.

add_metric(Metric, AllMetrics) when is_list(Metric)->
  add_nested_metric(AllMetrics, Metric, Metric).

%% Please note that it would've been more elegant to pattern-match
%% in function args instead of case metric elements but it doesn't work with maps
%% due to non-guaranteed order of argument resolution

%% Reached the end of metric-name list, create new counter ref
add_nested_metric(Metrics, Metric, [Last]) when is_map(Metrics)->
  case Metrics of
    #{Last := ExistingCounter} when is_reference(ExistingCounter) ->
      lager:error("Duplicate entry in metrics config: ~p!  Exiting...", [Metric]),
      true = false;
    _ ->
      lager:info("Adding metric ~p", [Metric]),
      Metrics#{Last => oneup:new_counter()}
  end;
add_nested_metric(Metrics, Metric, [Head | Tail]) when is_map(Metrics)->
  case Metrics of
    %% Existing entry
    #{ Head := NestedMetrics} -> Metrics#{Head => add_nested_metric(NestedMetrics, Metric, Tail)};
    %% First encounter of this entry
    _ -> Metrics#{Head => add_nested_metric(#{}, Metric, Tail)}
  end.

increment(Metric)->
  update(Metric, fun(Ref)-> oneup:inc(Ref) end).

%% TODO rename increment/3
increment(Metrics, Metric) when is_map(Metrics)->
  update(Metrics, Metric, fun(Ref)-> oneup:inc(Ref) end);
increment(Metric, Value) when is_integer(Value)->
  update(Metric, fun(Ref) -> oneup:inc2(Ref, Value) end).

increment(Metrics, Metric, Value)->
  update(Metrics, Metric, fun(Ref) -> oneup:inc2(Ref, Value) end).

set(Metric, Value)->
  update(Metric, fun(Ref) -> oneup:set(Ref, Value) end).

set(Metrics, Metric, Value)->
  update(Metrics, Metric, fun(Ref) -> oneup:set(Ref, Value) end).

reset(Metric)->
  update(Metric, fun(Ref) -> oneup:set(Ref, 0) end).

reset(Metrics, Metric)->
  update(Metrics, Metric, fun(Ref) -> oneup:set(Ref, 0) end).

update(Metric, Fun) ->
  case config() of
    Metrics when is_map(Metrics)-> update(Metrics, Metric, Fun);
    _Other -> ok
  end.

update(Metrics, Metric, Fun)->
  case get_counter(Metrics, Metric) of
    {error, Error} -> lager:warning("Metric ~p error ~p", [Metric, Error]);
    Ref -> Fun(Ref)
  end.

get_counter(Metric)->
  get_counter(config(), Metric).

get_counter(Metrics, [Metric]) ->
  case Metrics of
    #{Metric := CounterRef} when is_reference(CounterRef) -> CounterRef;
    _-> {error, uninitialized}
  end;
get_counter(Metrics, [Head | Tail])->
  case Metrics of
    #{Head := NestedMetric} -> get_counter(NestedMetric, Tail);
    _-> {error, uninitialized}
  end.

get(Metric)->
  CounterRef = get_counter(Metric),
  oneup:get(CounterRef).

get(Metrics, Metric)->
  CounterRef = get_counter(Metrics, Metric),
  oneup:get(CounterRef).


%% Get entire nested map
get_metrics(Metrics, [Metric]) ->
  case Metrics of
    #{Metric := ExpectedMetric} -> ExpectedMetric;
    _-> {error, uninitialized}
  end;
get_metrics(Metrics, [Head | Tail])->
  case Metrics of
    #{Head := NestedMetric} -> get_metrics(NestedMetric, Tail);
    _-> {error, uninitialized}
  end.

reset_counters(CounterRef) when is_reference(CounterRef)->
  oneup:set(CounterRef, 0);
reset_counters(MetricsMap) ->
  maps:fold(fun(_Key, Val, _Acc) -> reset_counter(Val) end, 'N/A', MetricsMap).

reset_counter(Val) when is_reference(Val) ->
  oneup:set(Val, 0);
reset_counter(Val) when is_map(Val)->
  reset_counters(Val).


display_counters(CounterRef) when is_reference(CounterRef) ->
  integer_to_binary(oneup:get(CounterRef));
display_counters(MetricsMap) when is_map(MetricsMap)->
  list_to_binary(display_counters(MetricsMap, "", [])).

display_counters(MetricsMap, Body, CurrMetric) ->
  maps:fold(fun(Key, Val, Acc) -> display_counter(Key, Val, Acc, CurrMetric) end, Body, MetricsMap).

display_counter(Key, Val, Body, CurrMetric) when is_reference(Val) ->
  CurrMetricDisplay =
    case string:join(lists:reverse(CurrMetric), ".") of
      "" -> "";
      NotEmpty when length(NotEmpty) > 0 -> NotEmpty ++ "."
    end,
  Body ++ CurrMetricDisplay ++ atom_to_list(Key) ++ ": " ++ integer_to_list(oneup:get(Val)) ++ "\n";
display_counter(Key, Val, Body, CurrMetric) when is_map(Val)->
  display_counters(Val, Body, [atom_to_list(Key) | CurrMetric]).

