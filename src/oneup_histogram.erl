%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2017 12:16 PM
%%%-------------------------------------------------------------------
-module(oneup_histogram).
-author("iguberman").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {metrics}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(HistogramMetrics) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [HistogramMetrics], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([HistogramMetrics]) ->
  {ok, #state{metrics = HistogramMetrics}}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
