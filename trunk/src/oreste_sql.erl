%%%-------------------------------------------------------------------
%%% File    : oreste_dsn.erl
%%% Author  : Matteo Redaelli <matteo@matteo Dot redaelli AT libero DOT it>
%%% Description : 
%%%
%%% Created : 25 Aug 2011 by Matteo Redaelli <matteo@matteo Dot redaelli AT libero DOT it>
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

-module(oreste_sql).

-behaviour(gen_server).

%% API
-export([start_link/2, 
	 get_sql_statement/2,
	 reload_configuration/1,
	 status/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {name="", folder="", statements=[], requests=0}).

%%-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
get_sql_statement(Name, Command) ->
    gen_server:call(Name, {get_sql_statement, Command}).

reload_configuration(Name) ->
    gen_server:cast(Name, {reload_configuration}).

start_link(Name,Folder) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name,Folder], []).

status(Name) ->
    gen_server:call(Name, {status}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Name, Folder]) ->
    State = #state{name=Name, folder=Folder},
    NewState = load_sql_file(State),	    
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_sql_statement, Command}, _From, State) ->
    case Reply = proplists:get_value(Command, State#state.statements) of
	undefined ->
	    error_logger:error_msg("Command ~p not found SQL in file ~p~n", 
				   [Command, State#state.name]);
	Reply ->
	    error_logger:info_msg("Command ~p found in SQL file ~p~n", 
				  [Command, State#state.name])
    end,
    NewState = State#state{requests = State#state.requests + 1},
    {reply, Reply, NewState};
handle_call({status}, _From, State) ->
    Reply = 
	"SQL name=" ++ atom_to_list(State#state.name) ++ 
	", statements=" ++  integer_to_list(length(State#state.statements)) ++
	", requests=" ++ integer_to_list(State#state.requests),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({reload_configuration}, State) ->
    NewState = load_sql_file(State),	    
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

load_sql_file(State) ->
    Name = State#state.name,
    Folder = State#state.folder,
    File = filename:join([Folder, atom_to_list(Name) ++ ".conf"]),

    error_logger:info_msg("Loading file ~p~n", [File]),
    case file:consult(File) of
	{ok, Statements} ->
	    error_logger:info_msg("Loaded file ~p: found ~p statements~n", [File, integer_to_list(length(Statements))]),
	    error_logger:info_msg("  SQL Statements: ~p~n", [ proplists:get_keys(Statements)]);
	{error, Reason} ->
	    error_logger:error_msg("Error loading file ~p, reason: ~p ~n", 
				   [File, Reason]),
	    Statements = []
    end,
    State#state{statements=Statements}.
