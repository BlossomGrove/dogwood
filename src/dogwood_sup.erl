%%%-------------------------------------------------------------------
%%% @author Johan <johan@yokohama>
%%% @copyright (C) 2016, Johan
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dogwood_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_main/0,start_child/1,stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_main() ->
    start_child(dogwood_manager).


start_child(Mod) ->
    Spec={Mod,{Mod,start_link,[]},permanent,2000,worker,[Mod]},
    case supervisor:start_child(?MODULE, Spec) of
	{ok,Child} ->
	    Child;
	{ok,Child,_Info} ->
	    Child;
	{error,{already_started,Pid}} ->
	    Pid;
	{error,Reason} ->
	    io:format("JB Reason=~p~n",[Reason]),
	    throw({error,Reason})
    end.

stop_child(Mod) ->
    supervisor:terminate_child(?MODULE,Mod).


%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
