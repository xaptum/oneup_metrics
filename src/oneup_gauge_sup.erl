%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 02. Feb 2018 2:51 PM
%%%-------------------------------------------------------------------
-module(oneup_gauge_sup).
-author("iguberman").


-behaviour(supervisor).

%% API
-export([start_link/0, start_gauge/2]).

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

  GaugeSpec =
    #{id => oneup_gauge,
      start => {oneup_gauge, start_link, []},
      restart => permanent,
      shutdown => 5000},

  {ok, {RestartStrategy, [GaugeSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_gauge(MetricName, Gauge) when is_atom(MetricName) ->
  supervisor:start_child(?MODULE, [MetricName, Gauge]).
