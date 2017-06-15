%%%-------------------------------------------------------------------
%%% @author johan <>
%%% @copyright (C) 2016, johan
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dogwood_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 read_cfg/0,
	 last/2,
	 event/1,event/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT,infinity).

-include("http.hrl").
-include("httpd.hrl").
-include("dogwood_internal.hrl").

-record(state,{
	 }).

%%%===================================================================
%%% API
%%%===================================================================
event({put,Channel,ReqHeaders,ReqBody}) ->
    gen_server:cast(?MODULE,{put,Channel,ReqHeaders,ReqBody});
event([Org,Event]) ->
    gen_server:cast(?MODULE,{Org,Event});
event(Event) ->
    gen_server:call(?MODULE,Event,?TIMEOUT).

event(Org,Event) ->
    gen_server:call(?MODULE,{Org,Event},?TIMEOUT).

last(User,SensorId) ->
    gen_server:call(?MODULE,{last,User,SensorId},?TIMEOUT).

read_cfg() ->
    gen_server:call(?MODULE,read_cfg,?TIMEOUT).
    

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
    %% Create a measurement for each sensor
%    Sensors=dogwood_lib:get_cfg(sensors,[]),
%    [dc_manager:create(Id) || #sensor{provider=Id} <- Sensors],

    {ok, #state{}}.

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
handle_call({post,_ReqHeaders,ReqBody}, _From, State) ->
    %% Got some sensor data, from the REST interface
    %% Should check Content-Type header..., but assume it is JSON coded for now.
    Body=emd_json:decode(ReqBody),
%    io:format("ReqHeaders=~p~n",[ReqHeaders]),
%    io:format("Body=~p~n",[Body]),
    handle_request(Body),

    D="<html>Got the data!</html>",
    RespHeaders=#http_response_h{content_type="text/html",
				 content_length=length(D)},
    {reply,{RespHeaders,D},State};
handle_call(read_cfg, _From, State) ->
    Resp=dogwood_lib:read_cfg(),
    dogwood_db:update(),
    {reply, Resp, State};
handle_call({last,User,SensorId}, _From, State) ->
    %% Currently we only have a single process handling access, should probably
    %% extend somehow...
    Resp=dogwood_access:last(User,SensorId),
    {reply, Resp, State};
handle_call(_Request, _From, State) ->
    io:format("_Request=~p~n",[_Request]),
    {reply, {error,bad_cmd}, State}.


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
handle_cast({mqtt,{connected,Id}}, State) ->
    io:format("MQTT ~p is now connected, subscribe to some topics!~n",[Id]),    
    SubscribeTopics=dogwood_lib:get_cfg(subscribe_topics),
    do_subscribe(Id,SubscribeTopics),

    {noreply,State};
handle_cast({mqtt,{disconnected,Id}}, State) ->
    io:format("MQTT ~p is now disconnected ~n",[Id]),    
    
    %% Trying to resubscribe... hmm
    SubscribeTopics=dogwood_lib:get_cfg(subscribe_topics),
    do_subscribe(Id,SubscribeTopics),

    {noreply,State};
handle_cast({mqtt,{publish,Topic,ReqBody}}, State) ->
    %% Got some data from the MQTT interface
    %% Content type info is missing from the MQTT protocol, but assume it is
    %% JSON coded for now...
    Body=try
	     emd_json:decode(ReqBody)
	 catch
	     Reason:_ ->
		 io:format("Not JSON: ~p~n",[Reason]),
		 ReqBody
	 end,
	     
    handle_request({mqtt,Topic,Body}),

    io:format("Topic=~p~n Body=~p~n",[Topic,Body]),
    {noreply,State}.


do_subscribe(Id,SubscribeTopics) ->
    case proplists:get_value(Id,SubscribeTopics) of
	undefined -> ok;
	Topics -> do_subscribe2(Topics)
    end.


do_subscribe2([]) ->
    ok;
do_subscribe2([Topic|Rest]) ->
    emqttc_manager:subscribe(Topic),
    do_subscribe2(Rest).



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
terminate(_Reason,_State) ->
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

handle_request([{header,H},{event,[{recordData,SensorData}]}]) ->    
    UnitId=binary_to_list(proplists:get_value(unitid,SensorData)),
    io:format("(From REST) SensorData=~p~n",[SensorData]),
    %% Provider=2 == dc_ds=2 => dc_dev=2 => type={http,kaa}
    dogwood_access:incoming(UnitId,2,{H,SensorData});
handle_request({mqtt,Topic,Body}) ->
    %% Note that this is a topic we have requested a subscription on, via
    %% configuration. Thus, we should use that configuration here...
    UnitId=case string:tokens(binary_to_list(Topic),"/") of
	       ["hplus","loradata","debug",UnitStr,"json"] ->
		   UnitStr;
	       ["hplus","data","debug",UnitStr,"json"] ->
		   UnitStr;
	       _ ->
		   undefined
	   end,
    SensorData=proplists:get_value('Payload',Body,[]),
    io:format("(From MQTT) UnitId=~p~n",[UnitId]),
    %% Provider=3 == dc_ds=3 => dc_dev=3 => type={mqtt,combitech}
    dogwood_access:incoming(UnitId,3,SensorData);
handle_request(Other) ->
    io:format("Received unknown data ~p~n",[Other]),
    ok.

