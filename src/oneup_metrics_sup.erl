%%%-------------------------------------------------------------------
%% @doc oneup_metrics top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('oneup_metrics_sup').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(MetricsMap) ->
    lager:info("~p:start_link(~p)", [?MODULE, MetricsMap]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [MetricsMap]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([MetricsMap]) ->
    %% Restart Strategy
    RestartStrategy = {one_for_one, 4, 3600},

    OneupMetricsServer =
        #{id => oneup_metrics,
            start => {oneup_metrics, start_link, [MetricsMap]},
            restart => permanent,
            shutdown => 1000},

    {ok, { RestartStrategy, [
        OneupMetricsServer]} }.

%%====================================================================
%% Internal functions
%%====================================================================
