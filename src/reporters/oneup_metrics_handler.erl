%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2017 3:14 PM
%%%-------------------------------------------------------------------
-module(oneup_metrics_handler).
-author("iguberman").

%% Cowboy API
-export([init/2]).
-export([content_types_provided/2]).
-export([
  process_request/2
]).

%% display utils API
-export([display_metrics/1,
  display_metric_name/1]).


init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

%% Req example: #{bindings => #{}, body_length => 0,has_body => false,
%%   headers => #{<<"accept">> => <<"*/*">>,<<"host">> => <<"localhost:8183">>,<<"user-agent">> => <<"curl/7.43.0">>},
%%   host => <<"localhost">>,host_info => undefined,method => <<"GET">>,
%%   path => <<"/metrics">>,path_info => undefined,peer => {{127,0,0,1},54931},pid => <0.26320.5>,
%%   port => 8183,qs => <<>>,ref => http,scheme => <<"http">>,streamid => 1,version => 'HTTP/1.1'} } = Req
content_types_provided(Req, State) ->
  {[
    {<<"text/plain">>, process_request}
  ], Req, State}.

process_request(Req, [] = _State) ->
  process_request(Req, [oneup_metrics:initial_get_config()]);
process_request(#{path_info := [<<"reset">>|PathBinTail]} = Req, [MetricsMap] = State) when is_map(MetricsMap) ->
  RespBody = apply_to_metrics(PathBinTail, MetricsMap, fun oneup_metrics:reset_counters/1),
  {RespBody, Req, State};
process_request(#{path_info := PathBinList} = Req, [MetricsMap] = State) when is_map(MetricsMap) ->
  RespBody = apply_to_metrics(PathBinList, MetricsMap, fun oneup_metrics_handler:display_metrics/1),
  {RespBody, Req, State}.

apply_to_metrics(undefined, MetricsMap, Fun)->
  Fun(MetricsMap);
apply_to_metrics([], MetricsMap, Fun)->
  Fun(MetricsMap);
apply_to_metrics(PathBinList, MetricsMap, Fun)->
    try [binary_to_existing_atom(P, utf8) || P <- PathBinList] of
      PathAtomList when is_list(PathAtomList) ->
        case oneup_metrics:get_metrics(MetricsMap, PathAtomList) of
          SubMetricsMap -> Fun(SubMetricsMap);
          {error, uninitialized} -> <<"Resource unavailable ">>
        end;
      PathAtomList -> print_invalid_request(PathAtomList)
    catch
      Error:Error -> print_invalid_request(Error)
    end.

print_invalid_request(Arg)->
  list_to_binary(lists:flatten(io_lib:format("Invalid request ~p", [Arg]))).

header()->
  oneup_gauge:header() ++ oneup_counter:header() ++ oneup_meter:header() ++ oneup_histogram:header().

display_metric_name(MetricName)->
  string:join([atom_to_list(Element) || Element <- MetricName], ".").

display_metrics(MetricsMap) when is_map(MetricsMap)->
  Body = header() ++ display_metrics(MetricsMap, "", []),
  lager:info("Displaying ~p", [Body]),
  list_to_binary(Body).

display_metrics(MetricsMap, Body, CurrMetricPrefix) ->
  maps:fold(fun(Key, Val, Acc) -> display_metric(Key, Val, Acc, CurrMetricPrefix) end, Body, MetricsMap).

display_metric(Key, {MetricType, Counters}, Body, CurrMetricPrefix)  ->
  MetricName = CurrMetricPrefix ++ [Key],
  Body ++ oneup_metrics:display(MetricName);
display_metric(Key, Val, Body, CurrMetricPrefix) when is_map(Val)->
  display_metrics(Val, Body, CurrMetricPrefix ++ [Key]).
