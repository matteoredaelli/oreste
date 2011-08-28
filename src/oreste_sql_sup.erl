%%%-------------------------------------------------------------------
%%% File    : oreste_sql_sup.erl
%%% Author  : Matteo Redaelli <matteo@matteo Dot redaelli AT libero DOT it>
%%% Description : 
%%%
%%% Created : 26 Aug 2011 by Matteo Redaelli <matteo@matteo Dot redaelli AT libero DOT it>
%%%-------------------------------------------------------------------

%% Copyright (C) 2010,2011 ~ matteo DOT redaelli AT libero DOT it
%% http://www.redaelli.org/matteo/
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.
%%
%% 
%% project url: http://code.google.com/p/oreste

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
    error_logger:info_msg("~p: starting the SQL children: ~p ~n", [?MODULE, string:join(SqlFiles, ", ")]),
    ChildrenSpecs = lists:map(
		      fun(Name) ->
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

