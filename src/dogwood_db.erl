%%%-------------------------------------------------------------------
%%% @author johan <>
%%% @copyright (C) 2016, johan
%%% @doc
%%% Just simple dets for now and improve if necessary
%%% Organize the data in 5 tables where 4 are holding queues with messages and
%%  and the 5:th holds the actual data.
%%% @end
%%%-------------------------------------------------------------------
-module(dogwood_db).

%% API
-export([open/1,close/1,
	 clean/0,
	 update/2,
	 insert_in_list/3
	]).

-include("dogwood_internal.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% Open a database.
open(Sensors) ->
    Accounts=dogwood_lib:get_cfg(accounts,[]),
    Db=ets:new(dogwood,[{keypos,#sensor.unit}]),
    ets:insert(Db,Sensors),
    Db.


close(Db) ->
    dets:close(Db).

clean() ->
    FileName=filename:join(em_lib:get_cfg(db_path),"dogwood.dets"),
    {ok,Db}=dets:open_file(FileName,[]),
    dets:delete_all_objects(Db).


update(Sensors,Db) ->
    Accounts=dogwood_lib:get_cfg(accounts,[]),
    ets:insert(Db,Sensors).



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================





%% Insert Ref in the sorted list List such that the last (largest) Ref is first
insert_in_list([],Ref,Out) ->
    lists:reverse([Ref|Out]);
insert_in_list([H|Rest],Ref,Out) when Ref<H ->
    insert_in_list(Rest,Ref,[H|Out]);
insert_in_list([H|Rest],Ref,Out) ->
    lists:reverse(Out)++[Ref,H|Rest].



