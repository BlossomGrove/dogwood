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
-export([open/0,close/1,
	 clean/0,
	 insert/2,
	 insert_in_list/3,

	 lookup_user/3
	]).

-record(dogwood_db,{
	  channel, % (ets) Keeps message references sorted on channels
	  user,    % (ets) Keeps message references sorted on users
	  tags,    % (ets) Keeps message references sorted on tags
	  mentions,% (ets) Keeps message references sorted on mentions
	  messages % (dets) The actual storage of messages
	 }).

-include("dogwood_internal.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% Open a database.
open() ->
    Accounts=dogwood_lib:get_cfg(accounts,[]),
    Sensors=dogwood_lib:get_cfg(sensors,[]),
    Db=ets:new(dogwood,[{keypos,#sensor.unit}]),
    ets:insert(Db,Sensors),
    Db.


close(Db) ->
    dets:close(Db).

clean() ->
    FileName=filename:join(em_lib:get_cfg(db_path),"dogwood.dets"),
    {ok,Db}=dets:open_file(FileName,[]),
    dets:delete_all_objects(Db).


insert(Msg,DogwoodDb=#dogwood_db{messages=Db}) ->
    MsgRef=dets:update_counter(Db,msg_ref,1),
    dets:insert(Db,{MsgRef,Msg}).

%% Lookup last N message in Db from User
lookup_user(User,N,#dogwood_db{user=Udb,
			      messages=Db}) ->
    case ets:lookup(Udb,User) of
	[] ->
	    empty;
	[{_,MsgRefList}] ->
	    MsgRefList
    end.


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



