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

    %%% TODO oneup_histogram and oneup_meter servers will also be children of this supervisor
    %%% if applicable depending on whether there are histograms and/or meters in config
%%    Histograms = oneup_metrics:get_histograms(MetricsMap),
%%    Meters = oneup_metrics:get_meters(MetricsMap),

    OneupMetricsServer =
        #{id => oneup_metrics,
            start => {oneup_metrics, start_link, [MetricsMap]},
            restart => permanent,
            shutdown => 1000},

    OneupCounterSup =
        #{id => oneup_counter_sup,
            start => {oneup_counter_sup, start_link, []},
            type => supervisor,
            restart => permanent,
            shutdown => 1000},

    OneupMeterSup =
        #{id => oneup_meter_sup,
            start => {oneup_meter_sup, start_link, []},
            type => supervisor,
            restart => permanent,
            shutdown => 1000},

    OneupGaugeSup =
        #{id => oneup_gauge_sup,
            start => {oneup_gauge_sup, start_link, []},
            type => supervisor,
            restart => permanent,
            shutdown => 1000},

    OneupHistogramSup =
        #{id => oneup_histogram_sup,
            start => {oneup_histogram_sup, start_link, []},
            type => supervisor,
            restart => permanent,
            shutdown => 1000},

    {ok, { RestartStrategy, [
        OneupMetricsServer,
        OneupCounterSup,
        OneupMeterSup,
        OneupGaugeSup,
        OneupHistogramSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================
