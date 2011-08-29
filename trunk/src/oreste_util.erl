%%%-------------------------------------------------------------------
%%% File    : oreste_util.erl
%%% Author  : Matteo Redaelli <matteo DOT redaelli AT libero DOT it>
%%% Description : 
%%%
%%% Created : 29 Aug 2011 by Matteo Redaelli
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

-module(oreste_util).

%% API
-export([which_children_names/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

which_children_names(SupName) ->
    Children = supervisor:which_children(SupName),
    lists:map( fun({Name,_,_,_}) -> Name end, Children).

%%====================================================================
%% Internal functions
%%====================================================================
