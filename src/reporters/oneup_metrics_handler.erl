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
  RespBody = apply_to_metrics(PathBinList, MetricsMap, fun oneup_metrics:display_counters/1),
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
  CounterHeader = lists:flatten(io_lib:format("~-15s~-50s~-20s~n", ["counter", "", "count"])),
  MeterHeader = lists:flatten(io_lib:format("~-15s~-50s~-20s~-20s~-20s~-20s~-20s~-20s~n",
    ["meter", "", "count", "cur_rate", "1m_rate", "5m_rate", "15m_rate", "mean"])),
  HistogramHeader = lists:flatten(io_lib:format("~-15s~-50s~-20s~-20s~-20s~-20s~-20s~-20s~n",
    ["histogram", "", "samples", "min", "median", "p75", "p99.9", "max"])),
  CounterHeader ++ MeterHeader ++ HistogramHeader.

format_metrics(Level, [], metrics)->
  lists:flatten(io_lib:format("~n", [])) ++ [format(Stat, Level) || Stat <- metrics];
format_metrics(Level, Name, metrics) when is_list(metrics) ->
  lists:flatten(io_lib:format("~-15s~s~s:~n", ["", tabs(Level), Name])) ++ [format(Stat, Level) || Stat <- metrics].

%% COUNTER
format({StatName,[
  {value,Value},
  {ms_since_reset,_MsSinceReset}]}, Level)->
  lists:flatten(io_lib:format("~-15s~-50s~-20b~n", ["counter", stat_display_name(StatName, Level), Value]));
%% METER
format({StatName, [
  {count, Count},
  {instant,InstantRate},
  {one,OneMinuteRate},
  {five,FiveMinuteRate},
  {fifteen,FifteenMinuteRate},
  {day,_DayRate},
  {mean,Mean},
  {acceleration,[{instant_to_one, _InstantToOneFloat},
    {one_to_five,_OneToFiveFloat},
    {five_to_fifteen,_FiveToFifteenFloat},
    {one_to_fifteen,_OneToFifteenFLoat}]}]}, Level) ->
  lists:flatten(io_lib:format("~-15s~-50s~-20b~-20b~-20b~-20b~-20b~-20b~n",
    ["meter", stat_display_name(StatName, Level),
      Count, Mean, InstantRate, OneMinuteRate, FiveMinuteRate, FifteenMinuteRate]));
format({StatName,[
  {n,Samples},{mean,_Mean},{min,Min},{max,Max},{median,Median},{50,_P50th},{75,P75th},{90,_P90th},{95,_P95th},{99,_P99th},{999,P999th}]}, Level)->
  lists:flatten(io_lib:format("~-15s~-50s~-20b~-20b~-20b~-20b~-20b~-20b~n",
    ["histogram", stat_display_name(StatName, Level),
      Samples, Min, Median, P75th, P999th, Max]));
format(Other, _Level)->
  lager:warning("Unexpected Stat: ~p", [Other]).

%% StatName is a list of atoms
stat_display_name(StatName, Level) when is_list(StatName) ->
  tabs(Level) ++ string:join([atom_to_list(A) || A <- lists:nthtail(Level, StatName)], ".").

tabs(Level)->
  string:copies("   ", Level).