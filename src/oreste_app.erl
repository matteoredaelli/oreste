%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the oreste application.

-module(oreste_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for oreste.
start(_Type, _StartArgs) ->
    oreste_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for oreste.
stop(_State) ->
    ok.
