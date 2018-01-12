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

-behaviour(oneup_metrics).

%% API
-export([
  init_metric/1,
  update/1,
  update/2,
  reset/1,
  get/1]).

init_metric(MetricName)->
  {?MODULE, oneup:new_counter()}.

update({?MODULE, CounterRef})->
  oneup:inc(CounterRef).

update({?MODULE, CounterRef}, Value) when is_integer(Value) ->
  oneup:inc2(CounterRef, Value).

reset({?MODULE, CounterRef}) ->
  oneup:set(CounterRef, 0).

get({?MODULE, CounterRef}) ->
  oneup:get(CounterRef).






