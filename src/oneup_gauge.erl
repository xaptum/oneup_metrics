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

-behaviour(oneup_metrics).

%% API
-export([
  init/0,
  update/1,
  update/2,
  reset/1,
  get/1]).

init()->
  {?MODULE, oneup:new_counter()}.

%% This method doesn't make a lot of sense for gauges
update({?MODULE, CounterRef})->
  oneup:set(CounterRef, 1).

update({?MODULE, CounterRef}, Value) when is_integer(Value) ->
  oneup:set(CounterRef, Value).

reset({?MODULE, CounterRef}) ->
  oneup:set(CounterRef, 0).

get({?MODULE, CounterRef}) ->
  oneup:get(CounterRef).
