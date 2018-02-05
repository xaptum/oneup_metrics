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

-define(INTERVAL_MILLIS, 60000).

-define(UNDEFINED_MIN, 999999999999).

%% API
-export([start_link/2]).

%% oneup_metrics callbacks
-export([
  init_metric/1,
  update/1,
  update/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {prev_value = 0, prev_samples = 0, value_aggr, samples, min, max}).

%%%===================================================================
%%% oneup_metrics API
%%%===================================================================

init_metric(MetricName)->
  Counters = [
    _ValueAggregateCounterRef = oneup:new_counter(),
    _OccurenceCounterRef = oneup:new_counter(),
    MinCounterRef = oneup:new_counter(),
    _MaxCounterRef = oneup:new_counter()
  ],
  oneup:set(MinCounterRef, ?UNDEFINED_MIN),
  oneup_histogram_sup:start_histogram(MetricName, Counters),
  {?MODULE, Counters}.

%% This method doesn't make much sense for histograms
update(undefined) -> ok.

%% It would be great if this was an atomic operation, consider oneup_histogram NIFs
update([ValueAggregateCounterRef, OccurenceCounterRef, MinCounterRef, MaxCounterRef], Value) when is_integer(Value) ->
  oneup:inc2(ValueAggregateCounterRef, Value),
  oneup:inc(OccurenceCounterRef),
  oneup:set_min(MinCounterRef, Value),
  oneup:set_max(MaxCounterRef, Value).

%%%===================================================================
%%% gen_server API
%%%===================================================================

start_link(MetricName, Counters) ->
  gen_server:start_link({local, oneup_metrics:metric_name_to_atom(MetricName)}, ?MODULE, [Counters], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([[ValueAggregateCounterRef, OccurenceCounterRef, MinRef, MaxRef]]) ->
  erlang:start_timer(?INTERVAL_MILLIS, self(), tick),
  {ok, #state{value_aggr = ValueAggregateCounterRef, samples = OccurenceCounterRef, min = MinRef, max = MaxRef}}.

handle_call(get, _From, #state{prev_value = PrevAggrValue, prev_samples = PrevSamples,value_aggr = ValueAggregateCounterRef, samples = SampleCounterRef, min = MinRef, max = MaxRef} = State) ->
  Samples = oneup:get(SampleCounterRef),
  Mean = avg( (PrevAggrValue + prev_oneup:get(ValueAggregateCounterRef)), (PrevSamples + Samples)),
  Min = oneup:get(MinRef),
  Max = oneup:get(MaxRef),
  {reply, {Samples, Mean, Min, Max}, State};
handle_call(reset, _From, #state{prev_value = PrevAggrValue, prev_samples = PrevSamples,value_aggr = ValueAggregateCounterRef, samples = SampleCounterRef, min = MinRef, max = MaxRef} = State) ->
  oneup:set(SampleCounterRef, 0),
  oneup:set(ValueAggregateCounterRef, 0),
  oneup:set(MinRef, ?UNDEFINED_MIN),
  oneup:set(MaxRef, 0),
  {reply, ok, State#state{prev_value = 0, prev_samples = 0}}.


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _TimerRef, tick},
    #state{prev_value = PrevValue, prev_samples = PrevSamples, value_aggr = ValueAggregateCounterRef, samples = OccurenceCounterRef} = State) ->

  %% a bit of a DANGER zone as far as stats accuracy goes because these two statements aren't atomic
  %% TODO implement atomic histograph NIFs in oneup (i.e. atomic struct of two longs, one for values one for counts)
  Samples = oneup:set(OccurenceCounterRef, 0), %% get and reset sample count
  Value  = oneup:set(ValueAggregateCounterRef, 0), %% get and reset values

  {DecayedPrevValue, DecayedPrevSamples} = collapse_decayed(PrevValue, PrevSamples),
  {DecayedNewValue, DecayedNewSamples} = collapse_decayed(Value, Samples),

  NewPrevSamples = DecayedPrevSamples + DecayedNewSamples,
  NewPrevValue = avg(DecayedPrevValue + DecayedNewValue, NewPrevSamples),

  {noreply, #state{prev_value = NewPrevValue, prev_samples = NewPrevSamples}}.

terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

min(?UNDEFINED_MIN)->
  0;
min(MinValue)->
  MinValue.

avg(Value, 0)->
  0;
avg(Value, Count) when Count > 0->
  Value/Count.

collapse_decayed(Value, 0)->
  {0, 0};
collapse_decayed(Value, 1)->
  {Value, 1}; %% can't decay
collapse_decayed(Value, Count) when Count >= 2 ->
  DecayedCount = floor(Count / 2),
  DecayedValue = (Value/Count * DecayedCount),
  {DecayedValue, DecayedCount}.
