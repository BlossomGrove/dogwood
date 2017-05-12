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
	 event/1,
	 insert/4
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
	  db
	 }).

%%%===================================================================
%%% API
%%%===================================================================
event({put,Channel,ReqHeaders,ReqBody}) ->
    gen_server:cast(?MODULE,{put,Channel,ReqHeaders,ReqBody});
event(Event) ->
    gen_server:call(?MODULE,Event,?TIMEOUT).


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
    %% Open database
    Db=dogwood_db:open(),

    {ok, #state{db=Db}}.

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
handle_call({post,ReqHeaders,ReqBody}, _From, State=#state{db=Db}) ->
    %% Got some sensor data, assume it is JSON coded for now...
    Body=emd_json:decode(ReqBody),
    io:format("ReqHeaders=~p~n Body=~p~n",[ReqHeaders,Body]),
    handle_request(Body,Db),

    D="<html>Got the data!</html>",
    RespHeaders=#http_response_h{content_type="text/html",
				 content_length=length(D)},
    {reply,{RespHeaders,D},State};
handle_call({get_last_from_channel,Channel,Nstr}, _From, State=#state{db=Db}) ->
    N=to_integer(Nstr),
    D=case dogwood_db:lookup_channel(Channel,N,Db) of
	  MsgList  when is_list(MsgList) ->
	      format_msglist(MsgList,[]);
	  empty ->
	      "Channel "++Channel++" holds no messages"
      end,
    RespHeaders=#http_response_h{content_type="text/html",
				 content_length=length(D)},
    {reply,{RespHeaders,D},State};
handle_call({get_last_from_user,User,Nstr}, _From, State=#state{db=Db}) ->
    N=to_integer(Nstr),
    D=case dogwood_db:lookup_user(User,N,Db) of
	  MsgList  when is_list(MsgList) ->
	      format_msglist(MsgList,[]);
	  empty ->
	      "User "++User++" holds no messages"
      end,
    RespHeaders=#http_response_h{content_type="text/html",
				 content_length=length(D)},
    {reply,{RespHeaders,D},State};
handle_call({get_last_from_tag,Tag,Nstr}, _From, State=#state{db=Db}) ->
    N=to_integer(Nstr),
    D=case dogwood_db:lookup_tag(Tag,N,Db) of
	  MsgList when is_list(MsgList) ->
	      format_msglist(MsgList,[]);
	  empty ->
	      "Tag "++Tag++" has no messages"
      end,
    RespHeaders=#http_response_h{content_type="text/html",
				 content_length=length(D)},
    {reply,{RespHeaders,D},State};
handle_call({get_last_mention,User,Nstr}, _From, State=#state{db=Db}) ->
    N=to_integer(Nstr),
    D=case dogwood_db:lookup_mention(User,N,Db) of
	  MsgList when is_list(MsgList) ->
	      format_msglist(MsgList,[]);
	  empty ->
	      "User "++User++" has no mentions"
      end,
    RespHeaders=#http_response_h{content_type="text/html",
				 content_length=length(D)},
    {reply,{RespHeaders,D},State};
handle_call(_Request, _From, State) ->
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
handle_cast({put,Channel,ReqHeaders,ReqBody}, State=#state{db=Db}) ->
    %% Parse and store a new message in the database
    %% As parsing and storing in a database are potential heavy opertaions
    %% spawn a dedicated proccess for the job and return the manager process.
    spawn_link(?MODULE,insert,[Channel,ReqHeaders,ReqBody,Db]),

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
terminate(_Reason,#state{db=Db}) ->
    dogwood_db:close(Db),
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
handle_request([{header,H},{event,[{recordData,RecordData}]}],Db) ->    
    Map=maps:from_list(RecordData),
    case lookup_sensor(maps:get(unitid,Map),Db) of
	undefined ->
	    ok;
	Sensor ->
	    Sensor
    end;
handle_request(Other,Db) ->
    io:format("Received unknown data ~p~n",[Other]),
    ok.


lookup_sensor(UnitId,Db) ->
    case ets:lookup(Db,UnitId) of
	[] ->
	    undefined;
	[Sensor] ->
	    Sensor
    end.





%% Callback for spawned storing process
insert(Channel,#http_request_h{other=Other},ReqBody,Db) ->
    io:format("Other=~p~n",[Other]),
    {Str,Tags,Mentions}=dogwood_lib:parse(ReqBody),
    User=proplists:get_value(user,Other),
    Msg=#msg{ts=erlang:timestamp(),
	     data=Str,
	     tags=Tags,
	     mentions=Mentions,
	     channel=Channel,
	     user=User
	    },
    io:format("Store ~p:~p~n",[Channel,Msg]),
    dogwood_db:insert(Msg,Db).


%% Format list of messages returned back to the user.
format_msglist([],Out) ->
    L=[% "<table style=\"width:800px\">"
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" "
       "\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
       "<html>"
       "<table>"
       "<tbody>",
       "<tr>"
       "<td style=\"text-align:left\">Time</td>"
       "<td style=\"text-align:left\">Message</td>"
       "<td style=\"text-align:left\">User</td>"
       "<td style=\"text-align:left\">Channel</td>"
       "<td style=\"text-align:left\">Tags</td>"
       "<td style=\"text-align:left\">Mentions</td></tr>",
%       "</thead>",
       lists:reverse(Out),
       "</tbody></table></html>"],
    lists:flatten(L);
format_msglist([#msg{ts=TS,user=User,channel=Channel,data=Data,
		     tags=Tags,mentions=Mentions}|Rest],Out) ->
    UserStr=if
		is_list(User) -> User;
		true -> "anonymous"
	    end,
    TagsStr=if
		Tags==[] -> "---";
		true -> format_strings(Tags,[])
	    end,
    MentionsStr=if
		Mentions==[] -> "---";
		true -> format_strings(Mentions,[])
	    end,
    H=["<tr>",
       "<td tt=\"ts\">",now_to_str(TS),"</td>",
       "<td tt=\"data\">"," : ",Data,"</td>",
       "<td tt=\"user\">"," : ",UserStr,"</td>",
       "<td tt=\"channel\">"," : ",Channel,"</td>",
       "<td tt=\"tags\">"," : ",TagsStr,"</td>",
       "<td tt=\"mentions\">"," : ",MentionsStr,"</td>",
       "</tr>"],
    format_msglist(Rest,[H|Out]).

now_to_str(undefined) ->
    "----";
now_to_str(Now) when is_tuple(Now) ->
    httpd_util:rfc1123_date(calendar:now_to_local_time(Now)).

format_strings([],[]) ->
    [];
format_strings([H],Out) ->
    lists:reverse([H|Out]);
format_strings([H|Rest],Out) ->
    format_strings(Rest,[",",H|Out]).



%% Expected an integer here, but just return a single message as backup 
to_integer(Nstr) when is_list(Nstr) ->
    case catch list_to_integer(Nstr) of
	N0 when is_integer(N0) -> N0;
	_ ->
	    1
    end;
to_integer(_) ->
    1.

