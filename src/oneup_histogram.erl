%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2017 12:16 PM
%%%-------------------------------------------------------------------
-module(oneup_histogram).
-author("iguberman").

-behaviour(gen_server).
-behaviour(oneup_metrics).

-define(INTERVAL_MILLIS, 10000).

%% API
-export([start_link/2]).

%% oneup_metrics callbacks
-export([
  init_metric/1,
  update/1,
  update/2,
  reset/1,
  get/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {value_aggr, occurences, min, max}).

%%%===================================================================
%%% oneup_metrics API
%%%===================================================================

init_metric(MetricName)->
  Counters = {
    ValueAggregateCounterRef = oneup:new_counter(),
    OccurenceCounterRef = oneup:new_counter(),
    MinCounterRef = oneup:new_counter(),
    MaxCounterRef = oneup:new_counter()
  },
  oneup_histogram_sup:start_histogram(MetricName, Counters),
  {?MODULE, Counters}.

%% This method doesn't make much sense for histograms
update(undefined) -> ok.

update({?MODULE, {ValueAggregateCounterRef, OccurenceCounterRef, MinCounterRef, MaxCounterRef}}, Value) when is_integer(Value) ->
  oneup:inc2(ValueAggregateCounterRef, Value),
  oneup:inc(OccurenceCounterRef),
  oneup:set_min(MinCounterRef, Value),
  oneup:set_max(MaxCounterRef, Value).

reset({?MODULE, ValueAggregateCounterRef, OccurenceCounterRef}) ->
  {oneup:set(ValueAggregateCounterRef, 0),
  oneup:set(OccurenceCounterRef, 1)}.

get(MetricName) ->
  gen_server:call(oneup_metrics:metric_name_to_atom(MetricName), get).

%%%===================================================================
%%% gen_server API
%%%===================================================================

start_link(MetricName, Counters) ->
  gen_server:start_link({local, oneup_metrics:metric_name_to_atom(MetricName)}, ?MODULE, [Counters], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([{ValueAggregateCounterRef, OccurenceCounterRef, MinRef, MaxRef}]) ->
  erlang:start_timer(?INTERVAL_MILLIS, self(), tick),
  {ok, #state{value_aggr = ValueAggregateCounterRef, occurences = OccurenceCounterRef, min = MinRef, max = MaxRef}}.

handle_call(get, _From, #state{value_aggr = ValueAggregateCounterRef, occurences = OccurenceCounterRef, min = MinRef, max = MaxRef} = State) ->
  ValueAggr = oneup:get(ValueAggregateCounterRef),
  Occurences = oneup:get(OccurenceCounterRef),
  Min = oneup:get(MinRef),
  Max = oneup:get(MaxRef),
  {reply, {Occurences, ValueAggr/Occurences, Min, Max}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _TimerRef, tick},
    #state{value_aggr = ValueAggregateCounterRef, occurences = OccurenceCounterRef} = State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
