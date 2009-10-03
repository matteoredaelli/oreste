%% @author author <matteo.redaelli@libero.it>
%% @copyright 2009 author.

%% @doc Callbacks for the oreste application.

-module(oreste_app).
-author('author <matteo.redaelli@libero.it>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for oreste.
start(_Type, _StartArgs) ->
    oreste_deps:ensure(),
    oreste_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for oreste.
stop(_State) ->
    ok.
