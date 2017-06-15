%%%-------------------------------------------------------------------
%%% @author Johan Blom <johan@localhost.localdomain>
%%% @copyright (C) 2017, Johan Blom
%%% @doc
%%%
%%% @end
%%% Created :  3 May 2017 by Johan Blom <johan@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(dogwood).

%% API
-export([start/0,stop/0,restart/0,
	 map_record/2]).

-include("dogwood_internal.hrl").


%%%===================================================================
%%% API
%%%===================================================================
%% erl -setcookie "secret" -name pilot@10.10.69.115 -pa meadow/ebin dogwood/ebin -run dogwood start
%% ps ao comm,args | grep ssh
%% ssh -R 10000:localhost:9999 dektech@kaa.testbed.se
start() ->
    dogwood_lib:read_cfg(),
    application:start(?MODULE),
    SiteHome="/opt/dogwood/site",
    jnets_server:start_servers(SiteHome).

stop() ->
    emd_lib:stop_app().


%% Ugly way to reread configuration
restart() ->
    application:stop(?MODULE),
    application:start(?MODULE).

%% --- Specific configuration records for dogwood
map_record(RecordType,Fields) ->
    case RecordType of
	account ->
	    DecFields=record_info(fields,account),
	    Defaults=[],
	    FieldList=emd_cfg:create_record_list(DecFields,Fields,Defaults,[]),
	    list_to_tuple([RecordType|FieldList]);
	sensor ->
	    DecFields=record_info(fields,sensor),
	    Defaults=[],
	    FieldList=emd_cfg:create_record_list(DecFields,Fields,Defaults,[]),
	    list_to_tuple([RecordType|FieldList])
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
