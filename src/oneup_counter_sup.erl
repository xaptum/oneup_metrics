%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 02. Feb 2018 3:33 PM
%%%-------------------------------------------------------------------
-module(oneup_counter_sup).
-author("iguberman").

-behaviour(supervisor).

%% API
-export([start_link/0, start_counter/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = {simple_one_for_one, 60, 3600},

  CounterSpec =
    #{id => oneup_counter,
      start => {oneup_counter, start_link, []},
      restart => temporary,
      shutdown => 5000},

  {ok, {RestartStrategy, [CounterSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_counter(MetricName, CounterRef) when is_atom(MetricName) ->
  supervisor:start_child(?MODULE, [MetricName, CounterRef]).
