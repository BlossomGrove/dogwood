%%%-------------------------------------------------------------------
%%% @author Johan <johan@yokohama>
%%% @copyright (C) 2016, Johan
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dogwood_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    dogwood_lib:read_cfg(),
    Apps=dogwood_lib:get_cfg(app_depend),

    case dogwood_sup:start_link() of
	{ok, Pid} ->
	    LibDir=filename:dirname(filename:dirname(
				      filename:dirname(code:which(?MODULE)))),
	    io:format("dogwood_app Apps=~p~n LibDir=~p~n",[Apps,LibDir]),
	    emd_lib:start_apps(Apps,LibDir),
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================