%%%-------------------------------------------------------------------
%%% @author Johan Blom <johan@localhost.localdomain>
%%% @copyright (C) 2017, Johan Blom
%%% @doc
%%%
%%% @end
%%% Created : 19 May 2017 by Johan Blom <johan@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(dogwood_access).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 incoming/3,
	 last/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("dogwood_internal.hrl").

-record(state,{
	  accounts,
	  sensor_cmd_db
	 }).


-record(sensor_cmd,{
	  unitid,
	  id,
	  provider,
	  actions
	 }).


-record(sensor_action,{
	  mfa,
	  keys
	  }).

%%%===================================================================
%%% API
%%%===================================================================
incoming(UnitId,Provider,Data) ->
    gen_server:cast(?MODULE,{incoming,UnitId,Provider,Data}).

last(User,SensorId) ->
    gen_server:call(?MODULE,{last,User,SensorId}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    Accounts=dogwood_lib:get_cfg(accounts,[]),
    Sensors=process_cfg(Accounts,[]),
    io:format("Accounts=~p~n Sensors=~p~n",[Accounts,Sensors]),
    SensorCmdDb=ets:new(sensor_cmd_db,[{keypos,#sensor_cmd.unitid}]),
    ets:insert(SensorCmdDb,Sensors),
    {ok, #state{accounts=Accounts,sensor_cmd_db=SensorCmdDb}}.

process_cfg([],Sens) ->
    Sens;
process_cfg([#account{fwds=Fwds,keys=Keys}|Rest],Sens) ->
    NewSens=process_fwd_cfg(Fwds,Keys,Sens),
    process_cfg(Rest,NewSens).


process_fwd_cfg([],_Keys,Sens) ->
    Sens;
process_fwd_cfg([{Sensors,FwdAction}|Rest],Keys,Sens) ->
    io:format("process_fwd_cfg Sensors=~p~n FwdAction=~p~n",[Sensors,FwdAction]),
    NewSens=process_fwdact(Sensors,FwdAction,Keys,Sens),
    process_fwd_cfg(Rest,Keys,NewSens).

process_fwdact([],_MFA,_Keys,Sens) ->
    Sens;
process_fwdact([SensorId|Rest],{M,F,Args0},Keys,Sens) ->
    %% Check SensorId in Sens
    case lists:keysearch(SensorId,#sensor_cmd.id,Sens) of
	false ->
    io:format("process_fwdact NEW SensorId=~p~n",[SensorId]),
	    case dogwood_db:lookup_sensor(SensorId) of
		#sensor{unit=UnitId,
			provider=Provider} ->
		    Args=process_args(Args0,UnitId,[]),
   io:format("process_fwdact NEW Args=~p~n",[Args]),
		    SensAction=#sensor_action{mfa={M,F,Args},
					      
					      keys=Keys},
		    NewSens=[#sensor_cmd{id=SensorId,
					 unitid=UnitId,
					 provider=Provider,
					 actions=[SensAction]}|Sens],
		    process_fwdact(Rest,{M,F,Args0},Keys,NewSens)
	    end;
	{value,SensCmd=#sensor_cmd{unitid=UnitId,actions=Actions}} ->
    io:format("process_fwdact OLD SensorId=~p~n",[SensorId]),
	    Args=process_args(Args0,UnitId,[]),
	    SensAction=#sensor_action{mfa={M,F,Args},
				      keys=Keys},
	    NewActions=[SensAction|Actions],
	    NewSensCmd=SensCmd#sensor_cmd{actions=NewActions},
	    NewSens=lists:keyreplace(SensorId,#sensor_cmd.id,Sens,NewSensCmd),
	    process_fwdact(Rest,{M,F,Args0},Keys,NewSens)
    end.


process_args([],_UnitId,Args) ->
    lists:reverse(Args);
process_args([Str|Rest],UnitId,Args) when is_list(Str) ->
    NewArg=process_args_str(Str,UnitId,[]),
    process_args(Rest,UnitId,[NewArg|Args]);
process_args([H|Rest],UnitId,Args) when is_atom(H) ->
    process_args(Rest,UnitId,[H|Args]);
process_args([H|Rest],UnitId,Args) ->
    process_args(Rest,UnitId,[H|Args]).

process_args_str([],_UnitId,Str) ->
    Str;
process_args_str("<thishost>",UnitId,Str) ->
    case string:tokens(atom_to_list(node()),"@") of
	[_,Host] ->
	    list_to_atom(Str++Host)
    end;
process_args_str("<unitid>"++Rest,UnitId,Str) ->
    process_args_str(Rest,UnitId,Str++UnitId);
process_args_str([H|Rest],UnitId,Str) ->
    process_args_str(Rest,UnitId,Str++[H]).


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
handle_call({last,_User,SensorId}, _From, State) ->
    %% Should validate that User has the right to access data from SensorId
    Resp=dc_manager:last(SensorId),
    {reply, Resp, State}.

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
handle_cast({incoming,UnitId,Provider,Data},
	    State=#state{sensor_cmd_db=SensorCmdDb}) ->
    %% 
    io:format("~p We have incoming data from ~p~n",[?MODULE,UnitId]),

    %% For simplicity, assume a 1-1 mapping between SensorId and Datasource Id
    case lookup_unitid(UnitId,Provider,SensorCmdDb) of
	undefined ->
	    io:format("~p NO ACTION~n",[?MODULE]);
	#sensor_cmd{id=SensorId,
		    actions=Actions} ->
	    StoreData=rewrite_sensordata(circdb,Provider,SensorId,Data),
    io:format("~p SensorId= ~p ==> ~p~n",[?MODULE,SensorId,StoreData]),
	    dc_manager:update(SensorId,StoreData),
%	    circdb_manager:update(SensorId,Data),
	    do_actions(Actions,Provider,SensorId,Data)
    end,
    {noreply, State}.



%% Format data depending on where it comes from (provider) and where it goes
%% (consumer).
%% Note:
%% - Now we handle this in a very hackish way by:
%%   + hard coding a few known possible providers
%%   + looking at some tag (eg protocol module) to decide the consumer
do_actions([],_Provider,_SensorId,_Data) ->
    ok;
do_actions([#sensor_action{mfa={M,F,A}}|Rest],Provider,SensorId,Data0) ->
    Args=rewrite_sensordata(M,Provider,SensorId,Data0),
    R=(catch apply(M,F,A++[Args])),
    io:format("~p:~p(~p) ==> ~p~n",[M,F,A++[Args],R]),
%    io:format("~p:~p(~p) ==> ~p~n",[M,F,A++[[SensorId,Data]],R]),
    do_actions(Rest,Provider,SensorId,Data0).


%% Each action requires Data formated in some way, thus we need to both input
%% format and output format.
%% FIXME: Handle this somehow
%% [{'MessageId',<<"1495471158217">>},
%%  {'AccountId',<<"debug">>},
%%  {'Payload',[{temperature,21.5},
%% 	      {humidity,31},
%% 	      {light,6},
%% 	      {motionCounter,0},
%% 	      {motion,false},
%% 	      {co2,438},
%% 	      {battery,3662}]}]
rewrite_sensordata(emqttc_manager,2,_SensorId,{H,Data}) ->
    [{long,LongMsgId}]=proplists:get_value(timestamp,H),
    MsgId=list_to_binary(integer_to_list(LongMsgId)),
    AccountId= <<"debug">>,
    Temp=proplists:get_value(temp,Data),
    Hum=proplists:get_value(humid,Data),
    Light=proplists:get_value(light,Data),
    MC=proplists:get_value(motion,Data),
    Motion=if
	       MC==0 -> false;
	       true -> true
	   end,
    CO2=proplists:get_value(co2,Data),
    Batt=proplists:get_value(batt,Data),
    PayLoad=[{temperature,Temp},
	     {humidity,Hum},
	     {light,Light},
	     {motionCounter,MC},
	     {motion,Motion},
	     {co2,CO2},
	     {battery,Batt}
	    ],
    JSON=[{'MessageId',MsgId},
	  {'AccountId',AccountId},
	  {'Payload',PayLoad}
	 ],
%    io:format("Preparing to publish: JSON=~p~n",[JSON]),
    list_to_binary(emd_json:encode(JSON));
%% Body=[{header,
%%            [{endpointKeyHash,[{string,<<"0B3BPu42uYFe9lo9H7ijjD+i7X4=">>}]},
%%             {applicationToken,[{string,<<"00395699408493415425">>}]},
%%             {headerVersion,[{int,1}]},
%%             {timestamp,[{long,1494502079956}]},
%%             {logSchemaVersion,[{int,4}]}]},
%%        {event,
%%            [{recordData,
%%                 [{unitid,<<"a81758fffe03078f">>},
%%                  {seq1,1294},
%%                  {rssi,-103},
%%                  {lsnr,9.3},
%%                  {temp,25},
%%                  {humid,15},
%%                  {acc,<<"0.0;0.0;0.0">>},
%%                  {light,981},
%%                  {motion,0},
%%                  {co2,597},
%%                  {batt,3667},
%%                  {analog1,0},
%%                  {gpsStr,<<"0:.0">>},
%%                  {timedatestr,<<"2017-05-11T11:27:59.143023">>}]}]}]
rewrite_sensordata(rpc,2,SensorId,{H,Data}) ->
    [SensorId,[H++Data]];
rewrite_sensordata(circdb,2,_SensorId,{_H,Data}) ->
    %% Only store co2 values for now...
    proplists:get_value(co2,Data).
    

lookup_unitid(UnitId,Provider,Db) ->
    io:format("lookup_unitid Provider=~p~n",[Provider]),
    case ets:lookup(Db,UnitId) of
	[SensCmd=#sensor_cmd{provider=Provider}] -> SensCmd;
	Other ->
    io:format("lookup_unitid Other=~p~n",[Other]),
	    undefined
    end.



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
