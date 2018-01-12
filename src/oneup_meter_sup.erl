%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2017 3:28 PM
%%%-------------------------------------------------------------------
-module(oneup_meter_sup).
-author("iguberman").

-behaviour(supervisor).

%% API
-export([start_link/0, start_meter/2]).

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

  meterSpec =
    #{id => oneup_meter,
      start => {oneup_meter, start_link, []},
      restart => temporary,
      shutdown => 5000},

  {ok, {RestartStrategy, [meterSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_meter(MetricName, Counter) when is_list(MetricName) ->
  supervisor:start_child(?MODULE, [MetricName, Counter]).