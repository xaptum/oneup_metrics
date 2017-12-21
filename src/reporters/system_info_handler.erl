%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 30. Nov 2017 8:45 PM
%%%-------------------------------------------------------------------
-module(system_info_handler).
-author("iguberman").

%% Cowboy API
-export([init/2]).
-export([content_types_provided/2]).
-export([process_request/2]).

-define(PROCESSES, <<"processes">>).
-define(PORTS, <<"ports">>).
-define(LARGE_MBOXES, <<"large_mboxes">>).
-define(MEMORY, <<"memory">>).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"text/plain">>, process_request}
  ], Req, State}.

%% TODO display more info
process_request(#{path_info := undefined} = Req, [{mbox_threshold, _MboxThreshold}] = State)->
  Body = show(?PROCESSES, State) ++ show(?PORTS, State) ++ show(?LARGE_MBOXES, State) ++ show(?MEMORY, State),
  {list_to_binary(Body), Req, State};
process_request(#{path_info := [Item]} = Req, [{mbox_threshold, _MboxThreshold}] = State) ->
  {list_to_binary(show(Item, State)), Req, State};
process_request(Req, State)->
  ct:print("Unsupported Request ~p, State ~p~n", [Req, State]),
  lager:warning("Unsupported Request ~p, State ~p", [Req, State]),
  {<<"Unsupported request">>, Req, State}.

show(?LARGE_MBOXES, [{mbox_threshold, MboxThreshold}] = State)->
  MboxSizeInfo = fun(Pid) ->
    case process_info(Pid, message_queue_len) of
      {message_queue_len, MboxSize} -> MboxSize;
      _Unknown -> 0
    end end,
  RegNameInfo = fun(Pid) ->
    case process_info(Pid, registered_name) of
      {registered_name, RegName} -> RegName;
      _Unknown -> Pid
    end end,
  LargeMboxes = [io_lib:format("     ~p: ~p~n", [RegNameInfo(Pid), Info]) || Pid <- processes(), (Info = MboxSizeInfo(Pid)) > MboxThreshold],
  "\nLarge Mboxes:\n" ++ lists:flatten(LargeMboxes);
show(?PROCESSES, _State) ->
  "\nProcesses: " ++ integer_to_list(length(erlang:processes()));
show(?PORTS, _State) ->
  "\nPorts: " ++ integer_to_list(length(erlang:ports()));
show(?MEMORY, _State)->
  "\nMemory:\n" ++ [lists:flatten(io_lib:format("    ~p: ~b~n", [Key, Value])) || {Key, Value} <- erlang:memory() ];
show(Other, _State)->
  lager:debug("Invalid system_info request: ~p", [Other]),
  "Invalid Request".

reg_name_info(Pid)->
  case process_info(Pid, registered_name) of
    {registered_name, RegName} -> RegName;
    _Unknown -> Pid
  end.

reg_name_match(Pid, Pattern) when is_list(Pattern), length(Pattern) > 0 ->
  case process_info(Pid, registered_name) of
    {registered_name, RegName} -> string:str(atom_to_list(RegName), Pattern);
    _Unknown -> false
  end;
reg_name_match(Pid, [])-> true.
