%%%-------------------------------------------------------------------
%%% @author zhaoxingu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2018 11:29 AM
%%%-------------------------------------------------------------------
-module(oneup_metric_config).
-author("zhaoxingu").

-define(TABLE_NAME, oneup_metrice_counter_refernce_record).
%% API
-export([init/0, start/0, insert/2, get/2,clear/0]).

init()->
  case foil_app:start() of
    {ok,_} -> case foil:new(?TABLE_NAME) of
                ok -> ok;
                _->lager:warning("oneup_metric_config failed to generate new table")
              end;
    _ ->lager:warning("oneup_metric_config failed to initialize")
  end.

start()->
  case foil:load(?TABLE_NAME) of
    ok -> ok;
    _->lager:warning("oneup_metric_config failed to start")
  end.

insert(Key,Value) when is_list(Key)->
  insert(metric_name_to_atom(Key),Value);
insert(Key,Value) when is_atom(Key)->
  foil:insert(?TABLE_NAME, Key, Value).



get(Key,Type) when is_list(Key)->
  get(metric_name_to_atom(Key),Type);
get(Key,Type) when is_atom(Key)->
  case foil:lookup(?TABLE_NAME, Key) of
  {ok, Counter_list} -> grab_ref(Counter_list, Type);
  {error, key_not_found} -> undefined
  end.


%%% internal function for getting counter reference from a list of tuple: {Type, Ref} base on Type
grab_ref([],Type)->
  undefined;
grab_ref([Counter_tuple],Type)->
  case Counter_tuple of
    {Type, Counters} -> case Type of
                          oneup_histogram ->convert_to_ref_histo(Counters);
                          oneup_counter -> convert_to_ref(Counters);
                          oneup_gauge -> convert_to_ref(Counters);
                          oneup_meter -> convert_to_ref(Counters);
                          _->Counters
                        end;
    {_,_}-> grab_ref([],Type)
  end;
grab_ref([Head|Tail],Type)->
  case Head of
    {Type, Counters} -> case Type of
                          oneup_histogram ->convert_to_ref_histo(Counters);
                          oneup_counter -> convert_to_ref(Counters);
                          oneup_gauge -> convert_to_ref(Counters);
                          oneup_meter -> convert_to_ref(Counters);
                          _->Counters
                        end;
    {_,_} -> grab_ref(Tail, Type)
  end.


metric_name_to_atom(MetricName)->
  list_to_atom(string:join([atom_to_list(Part) || Part <- MetricName], ".")).

clear()->
  ok = foil:delete(?TABLE_NAME),
  foil_app:stop().

convert_to_ref(Ref_list)->
  [Name] = Ref_list,
  [list_to_ref(Name)].

convert_to_ref_histo(Ref_list)->
  [Name_0,Name_1,Name_2,Name_3] = Ref_list,
  [list_to_ref(Name_0),list_to_ref(Name_1),list_to_ref(Name_2),list_to_ref(Name_3)].