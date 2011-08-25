%%%-------------------------------------------------------------------
%%% File    : oreste_dsn_sup.erl
%%% Author  : Matteo Redaelli <matteo@matteo-desktop>
%%% Description : 
%%%
%%% Created : 25 Aug 2011 by Matteo Redaelli <matteo@matteo-desktop>
%%%-------------------------------------------------------------------
-module(oreste_dsn_sup).

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
start_link(DsnFile) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [DsnFile]).

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
init([DsnFile]) ->
    {ok, Dsn_list} = file:consult(DsnFile),
    ChildrenSpecs = lists:map(
		      fun({Name,DSN}) ->
			      NameStr = atom_to_list(Name),
			      error_logger:info_msg("DSN: ~p~n", [NameStr]),
			      AChild = {Name,{oreste_dsn,start_link,[{Name,DSN}]},
					permanent,2000,worker,[oreste_dsn]},
			      AChild
		      end,
		      Dsn_list
		     ),
    {ok,{{one_for_one,0,1}, ChildrenSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

