%%%-------------------------------------------------------------------
%% @doc oneup_metrics public API
%% @end
%%%-------------------------------------------------------------------

-module('oneup_metrics_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  {ok, MetricsConfig} = application:get_env(metrics_config),

  MetricsMap = oneup_metrics:init_from_config(MetricsConfig),

  maybe_start_http_reporters(),

  lager:info("Starting oneup_metrics_sup with ~p", [MetricsMap]),
  oneup_metrics_sup:start_link(MetricsMap).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

maybe_start_http_reporters()->
  case application:get_env(http_port) of
    {ok, HttpPort} -> start_http_reporter(HttpPort);
    _ -> ok
  end.

start_http_reporter(HttpPort)->
  application:ensure_all_started(cowboy),
  {ok, SystemInfoConfig} = application:get_env(system_info_config),
  lager:info("Starting oneup stats http server on ~p with system_info_config ~p", [HttpPort, SystemInfoConfig]),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/system_info", system_info_handler, SystemInfoConfig},
      {"/system_info/[...]", system_info_handler, SystemInfoConfig},
      {"/", oneup_metrics_handler, []},
      {"/[...]", oneup_metrics_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, HttpPort}], #{env => #{dispatch => Dispatch}
  }).
