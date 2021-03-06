%%%-------------------------------------------------------------------
%%% File    : oreste_dsn.erl
%%% Author  : Matteo Redaelli <matteo@matteo Dot redaelli AT libero DOT it>
%%% Description : 
%%%
%%% Created : 24 Aug 2011 by Matteo Redaelli <matteo@matteo Dot redaelli AT libero DOT it>
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
%% project url: http://code.google.com/p/oreste

-module(oreste_dsn).

-behaviour(gen_server).

%% API
-export([
	 db_reconnect/1,
	 start_link/1, 
	 sql_query/2,
	 status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {name="", dsn="", db_options=[], dbConn={error,firstStart}, requests=0}).

%%-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

db_reconnect(Name) ->
    gen_server:call(Name, {db_reconnect}).

sql_query(Name, SQL) ->
    {ok, Timeout} = application:get_env(oreste, odbc_sql_query_timeout),
    error_logger:info_msg("sql_query timeout=~p~n", [Timeout]),
    gen_server:call(Name, {sql_query, SQL}, Timeout).

start_link({Name,DSN}) ->
    gen_server:start_link({local, Name}, ?MODULE, [{Name,DSN}], []).

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
init([{Name,{DSN, DBOptions}}]) ->
    State = #state{name=Name, dsn=DSN, db_options=DBOptions},
    NewState = db_connect(State),
    process_flag(trap_exit, true),
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
handle_call({db_reconnect}, _From, State) ->
    NewState = db_connect(db_disconnect(State)),
    {reply, ok, NewState};

handle_call({sql_query, SQL}, _From, State) ->
    DSN = State#state.dsn,
    case State#state.dbConn of
	{ok, Ref} ->
	    case Reply = odbc:sql_query(Ref, SQL) of
		{error, Reason} ->
		    error_logger:error_msg("Cannot run SQL to DB ~p, reason: ~p ~n", [DSN, Reason]),
		    error_logger:error_msg("Trying to reconnect to DB ~p ~n", [DSN]),
		    TmpState = db_connect(db_disconnect(State));
		Reply ->
		    TmpState = State
	    end;
	{error, Reason} ->
	    error_logger:error_msg("Cannot run SQL: not connected to DB ~p, reason: ~p ~n", [DSN, Reason]),
	    TmpState = db_connect(State),
	    Reply = ok
    end,
    NewState = TmpState#state{requests = TmpState#state.requests + 1},
    {reply, Reply, NewState};
handle_call({status}, _From, State) ->
    Reply = "DSN name=" ++ atom_to_list(State#state.name) ++ ", requests=" ++ integer_to_list(State#state.requests),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    NewState = db_disconnect(State),
    {stop, normal, ok, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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
terminate(shutdown, State) ->
    db_disconnect(State),
    ok;
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

db_connect(State) ->
    DSN = State#state.dsn,
    DbOptions = State#state.db_options,
    error_logger:info_msg("Connecting to DB ~p ...~n", [DSN]),
    DbConn = odbc:connect(DSN, DbOptions),
    case DbConn of
	{ok, _} ->
	    error_logger:info_msg("Connected to DB ~p with options ~p ~n", [DSN, DbOptions]);
	{error, Reason} ->
	    error_logger:error_msg("Error connecting to DB ~p, reason: ~p ~n", [DSN, Reason])
    end,
    State#state{dbConn = DbConn}.

db_disconnect(State) ->
    DSN = State#state.dsn,
    error_logger:info_msg("Disconnecting from DB ~p~n", [DSN]),
    DbConn = State#state.dbConn,
    case DbConn of
	{ok, Ref} ->
	    Result = odbc:disconnect(Ref);
	{error, _Reason} ->
	    Result = {ok, already_disconnected}
    end,
    error_logger:info_msg("Result: ~p~n", [Result]),
    State#state{dbConn = {error, disconnected}}.
