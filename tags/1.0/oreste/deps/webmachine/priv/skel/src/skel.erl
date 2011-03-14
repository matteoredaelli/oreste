%% @author author <matteo.redaelli@libero.it>
%% @copyright 2009 author.

%% @doc TEMPLATE.

-module(skel).
-author('author <matteo.redaelli@libero.it>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the skel server.
start() ->
    skel_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(skel).

%% @spec stop() -> ok
%% @doc Stop the skel server.
stop() ->
    Res = application:stop(skel),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
