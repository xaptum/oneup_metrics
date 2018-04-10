%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 3:47 PM
%%%-------------------------------------------------------------------
-module(oneup_gauge).
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

-record(state, {gauge, display_name}).


%%%===================================================================
%%% oneup_metrics callbacks
%%%===================================================================

init_metric(MetricName) when is_list(MetricName)->
  MetricNameAtom = oneup_metrics:metric_name_to_atom(MetricName),
  init_metric(MetricNameAtom);
init_metric(MetricName)->
  Gauge = oneup:new_counter(),
  lager:info("Starting gauge ~p", [MetricName]),
  oneup_metric_config:insert(MetricName,[{oneup_gaugr, Gauge}]),
  oneup_gauge_sup:start_gauge(MetricName, Gauge),
  {?MODULE, MetricName, Gauge}.

%% This method doesn't make a lot of sense for gauges
update(CounterRef)->
  oneup:set(CounterRef, 1).

update(CounterRef, Value) when is_integer(Value) ->
  oneup:set(CounterRef, Value).

header()->
  lists:flatten(io_lib:format("~-15s~-50s~-20s~n", ["gauge", "", "value"])).

%%%===================================================================
%%% gen_server API
%%%===================================================================

start_link(MetricName, GaugeRef) ->
  gen_server:start_link({local, MetricName}, ?MODULE, [MetricName, GaugeRef], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MetricName, GaugeRef]) ->
  {ok, #state{gauge = GaugeRef, display_name = atom_to_list(MetricName)}}.

handle_call(get, _From, #state{gauge = GaugeRef} = State) ->
  {reply, oneup:get(GaugeRef), State};
handle_call(reset, _From, #state{gauge = GaugeRef} = State) ->
  {reply, oneup:set(GaugeRef, 0), State};
handle_call(display, _From, #state{gauge = GaugeRef, display_name = DisplayName} = State) ->
  CounterValue = oneup:get(GaugeRef),
  DisplayString = io_lib:format("~-15s~-50s~-20b~n", ["gauge", DisplayName, CounterValue]),
  {reply, DisplayString, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Req, State)->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
