%%%-------------------------------------------------------------------
%%% File    : oreste_sql_sup.erl
%%% Author  : Matteo Redaelli <matteo@matteo-desktop>
%%% Description : 
%%%
%%% Created : 26 Aug 2011 by Matteo Redaelli <matteo@matteo-desktop>
%%%-------------------------------------------------------------------
-module(oreste_sql_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(SQLFolder) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [SQLFolder]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([Folder]) ->
    {ok, AllFiles} = file:list_dir(Folder),
    %% removing all not .conf files
    SqlFiles = lists:filter(fun(E) -> re:run(E, "\.conf\$") =/= nomatch end, 
			    AllFiles),
    %% removing string .conf$ 
    %% TODO: Files must start with lowercase: please check
    Names = lists:map(fun(F) -> [Name|_] = string:tokens(F, "."), list_to_atom(Name) end,
		     SqlFiles),

    ChildrenSpecs = lists:map(
		      fun(Name) ->
			      NameStr = atom_to_list(Name),
			      error_logger:info_msg("name: ~p~n", [NameStr]),
			      AChild = {Name,{oreste_sql,start_link,[Name,Folder]},
					permanent,2000,worker,[oreste_sql]},
			      AChild
		      end,
		      Names
		     ),
    {ok,{{one_for_one,0,1}, ChildrenSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

