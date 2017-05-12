%% coding: latin-1
%%%-------------------------------------------------------------------
%%% @author johan <>
%%% @copyright (C) 2016, johan
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dogwood_lib).

%% API
-export([parse/1,
	 read_cfg/0, get_cfg/1]).

-include("dogwood_internal.hrl").

%%%===================================================================
%%% API
%%%===================================================================

read_cfg() ->
    emd_cfg:read_cfg(?APP_NAME).

%% Access configuration
get_cfg(Cfg) ->
    emd_cfg:get_cfg_a(?APP_NAME,Cfg).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%% Parse a dogwood message 
parse(Bin) when is_binary(Bin) ->
    Str=binary_to_list(Bin),
    {Tags,Mentions}=parse_msg(Str,[],[]),
    {Str,Tags,Mentions}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_msg([],Tags,Mentions) ->
    {Tags,Mentions};
parse_msg([$#|Rest],Tags,Mentions) -> % Found a tag
    {Tag,NewRest}=parse_msg_tag(Rest,[]),
    NewTags=case lists:member(Tag,Tags) of
		false ->
		    [Tag|Tags];
		true ->
		    Tags
	    end,
    parse_msg(NewRest,NewTags,Mentions);
parse_msg([$@|Rest],Tags,Mentions) -> % Found a mention
    {Mention,NewRest}=parse_msg_mention(Rest,[]),
    NewMentions=case lists:member(Mention,Mentions) of
		    false ->
			[Mention|Mentions];
		    true ->
			Mentions
	    end,
    parse_msg(NewRest,Tags,NewMentions);
parse_msg([_|Rest],Tags,Mentions) ->
    parse_msg(Rest,Tags,Mentions).


parse_msg_mention([],Out) ->
    {lists:reverse(Out),[]};
parse_msg_mention([H|Rest],Out) when $A=<H,H=<$Z;
				     $a=<H,H=<$z;
				     $0=<H,H=<$9;
				     H==$Å;H==$Ä;H==$Ö;
				     H==$å;H==$ä;H==$ö ->
    parse_msg_mention(Rest,[H|Out]);
parse_msg_mention(Rest,Out) ->
    {lists:reverse(Out),Rest}.


parse_msg_tag([],Out) ->
    {lists:reverse(Out),[]};
parse_msg_tag([H|Rest],Out) when $A=<H,H=<$Z;
				 $a=<H,H=<$z;
				 $0=<H,H=<$9;
				 H==$Å;H==$Ä;H==$Ö;
				 H==$å;H==$ä;H==$ö ->
    parse_msg_tag(Rest,[H|Out]);
parse_msg_tag(Rest,Out) ->
    {lists:reverse(Out),Rest}.
