%%%-------------------------------------------------------------------
%%% @author Johan Blom <johan@localhost.localdomain>
%%% @copyright (C) 2017, Johan Blom
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2017 by Johan Blom <johan@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(dogwood_db).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([update/0,
	 lookup_unit/1,
	 lookup_sensor/1,
	 insert_in_list/3
	]).


-define(SERVER, ?MODULE).
-define(TIMEOUT,infinity).

-record(state,{
	  sensor_db
	 }).



-include("dogwood_internal.hrl").


%%%===================================================================
%%% API
%%%===================================================================

update() ->
    gen_server:cast(?MODULE,update).

lookup_unit(UnitId) ->
    gen_server:call(?MODULE,{lookup_unit,UnitId},?TIMEOUT).

lookup_sensor(SensorId) ->
    gen_server:call(?MODULE,{lookup_sensor,SensorId},?TIMEOUT).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Sensors) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Sensors], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Sensors]) ->
    process_flag(trap_exit, true),
    SensorDb=ets:new(dogwood,[{keypos,#sensor.unit}]),
    ets:insert(SensorDb,Sensors),
    {ok, #state{sensor_db=SensorDb}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({lookup_unit,UnitId},_From,State=#state{sensor_db=SensorDb}) ->
    Reply = lookup_unit(UnitId,SensorDb),
    {reply, Reply, State};
handle_call({lookup_sensor,SensorId},_From,State=#state{sensor_db=SensorDb}) ->
    Reply = lookup_sensor(SensorId,SensorDb),
    {reply, Reply, State}.


lookup_unit(UnitId,Db) ->
    case ets:lookup(Db,UnitId) of
	[] ->
	    undefined;
	[Sensor] ->
	    Sensor
    end.

lookup_sensor(SensorId,Db) ->
    io:format("SensorId=~p~n Resuls=~p~n Db=~p~n",
	      [SensorId,
	       ets:match(Db,#sensor{id=SensorId,_= '_'}),
	       ets:tab2list(Db)]),
    case ets:match_object(Db,#sensor{id=SensorId,_= '_'}) of
	[] ->
	    undefined;
	[Sensor] ->
	    Sensor
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(update, State=#state{sensor_db=SensorDb}) ->
    Sensors=dogwood_lib:get_cfg(sensors,[]),
    ets:insert(SensorDb,Sensors),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
