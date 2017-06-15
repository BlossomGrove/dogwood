%%%-------------------------------------------------------------------
%%% @author johan <>
%%% @copyright (C) 2016, johan
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by johan <>
%%%-------------------------------------------------------------------
-module(mod_dogwood).

%% API
-export([do/1]).

-include("httpd.hrl").
-include("http.hrl").

%%%===================================================================
%%% API
%%%===================================================================
do(Info) ->
    case httpd_util:response_generated(Info) of
	{no_response,Method,Path,Data,ConfigDb} ->
	    do_dogwood(Method,Path,Data,ConfigDb,Info);
	Proceed ->
	    Proceed
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Spawn a proccess for each incoming command and 
do_dogwood(Method,Path,Data,ConfigDb,
	   #mod{headers=ReqHeaders,entity_body=ReqBody}) ->
    Cmd=case string:tokens(Path,"/") of
	    ["post"] -> {post,ReqHeaders,ReqBody};

	    %% These are not in use ....
	    ["put",Channel] ->		   {put,Channel,ReqHeaders,ReqBody};
	    ["get","last_ch",Channel,N] -> {get_last_from_channel,Channel,N};
	    ["get","last_us",User,N] ->    {get_last_from_user,User,N};
	    ["get","last_ta",Tag,N] ->     {get_last_from_tag,Tag,N};
	    ["get","last_me",User,N] ->    {get_last_mention,User,N};
	    Other ->
		io:format("Unsupported command: ~p~n",[Other]),
		Other
	end,
    case dogwood_manager:event(Cmd) of
	ok ->
	    io:format("JB-1 ~n",[]),
	    RespBody="Ok",
	    RespHeaders=#http_response_h{content_type="text/html",
					 content_length=length(RespBody)},
	    {proceed,[{response,{200,RespHeaders,RespBody}}|Data]};	    
	{error,Reason} ->
	    io:format("JB-2 Reason=~p ~n",[Reason]),
	    Str=io_lib:format("~p is not a valid command, got ~p",[Cmd,Reason]),
	    RespBody=lists:flatten(Str),
	    RespHeaders=#http_response_h{content_type="text/html",
					 content_length=length(RespBody)},
	    {proceed,[{response,{200,RespHeaders,RespBody}}|Data]};
	{RespHeaders,RespBody} ->
	    {proceed,[{response,{200,RespHeaders,RespBody}}|Data]}
    end.
