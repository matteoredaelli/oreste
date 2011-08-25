%%%-------------------------------------------------------------------
%%% File    : oreste_dsn.erl
%%% Author  : Matteo Redaelli <matteo@matteo-desktop>
%%% Description : 
%%%
%%% Created : 24 Aug 2011 by Matteo Redaelli <matteo@matteo-desktop>
%%%-------------------------------------------------------------------
-module(oreste_dsn).

-behaviour(gen_server).

%% API
-export([start_link/1, 
	 sql_query/2,
	 stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {name="", dsn="", dbConn={error,firstStart}}).

%%-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
sql_query(Name, SQL) ->
    gen_server:call(Name, {sql_query, SQL}).

start_link({Name,DSN}) ->
    gen_server:start_link({local, Name}, ?MODULE, [{Name,DSN}], []).

stop(Name) -> gen_server:call(Name, stop).
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
init([{Name,DSN}]) ->
    error_logger:info_msg("Connecting to DB ~p ...~n", [DSN]),
    State = #state{name=Name, dsn=DSN},
    NewState = db_connect(State),	    
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
handle_call({sql_query, SQL}, _From, State) ->
    DSN = State#state.dsn,
    case State#state.dbConn of
	{ok, Ref} ->
	    case Reply = odbc:sql_query(Ref, SQL) of
		{error, Reason} ->
		    error_logger:error_msg("Cannot run SQL to DB ~p, reason: ~p ~n", [DSN, Reason]),
		    error_logger:error_msg("Trying to reconnect to DB ~p ~n", [DSN]),
		    TmpState = db_disconnect(State),
		    NewState = db_connect(TmpState);
		Reply ->
		    NewState = State
	    end;
	{error, Reason} ->
	    error_logger:error_msg("Cannot run SQL: not connected to DB ~p, reason: ~p ~n", [DSN, Reason]),
	    NewState = db_connect(State),
	    Reply = ok
    end,
    {reply, Reply, NewState};
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
    DbConn = odbc:connect(DSN, [{timeout, 5000}, {scrollable_cursors, off}]),
    case DbConn of
	{ok, _} ->
	    error_logger:info_msg("Connected to DB ~p~n", [DSN]);
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
