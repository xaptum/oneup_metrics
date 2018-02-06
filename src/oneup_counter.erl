%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 3:37 PM
%%%-------------------------------------------------------------------
-module(oneup_counter).
-author("iguberman").

-behavior(gen_server).
-behaviour(oneup_metrics).


%% API
-export([
  start_link/2,
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

-record(state, {counter}).

init_metric(MetricName)->
  CounterRef = oneup:new_counter(),
  oneup_counter_sup:start_counter(MetricName, CounterRef),
  {?MODULE, CounterRef}.

update(CounterRef)->
  oneup:inc(CounterRef).

update(CounterRef, Value) when is_integer(Value) ->
  oneup:inc2(CounterRef, Value).

%%%===================================================================
%%% gen_server API
%%%===================================================================

start_link(MetricName, CounterRef) ->
  gen_server:start_link({local, oneup_metrics:metric_name_to_atom(MetricName)}, ?MODULE, [CounterRef], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([CounterRef]) ->
  {ok, #state{counter = CounterRef}}.

handle_call(get, _From, #state{counter = CounterRef} = State) ->
  {reply, oneup:get(CounterRef), State};
handle_call(reset, _From, #state{counter = CounterRef} = State) ->
  {reply, oneup:set(CounterRef, 0), State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Req, State)->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.






