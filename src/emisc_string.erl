%%%-------------------------------------------------------------------
%%% File    : emisc_string.erl
%%% Author  : matteo <matteo@nowar>
%%% Description : functions we would like to have in the official 
%%%               erlang module
%%%
%%% Created : 20 Sep 2009 by matteo <matteo.redaelli@libero.it>
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


-module(emisc_string).
-export([
	 join_to_fixed/3,
	 split_inclusive/2
	]).

%%%-------------------------------------------------------------------
%%% Fuction     : join_to_fixed
%%% Description : like join but output in a fixed text file 
%%%               the list of lengths is the second parameter
%%% example 
%%% emisc_string:join_to_fixed(["casa", "list", "aa"], [7,6,3], $.).
join_to_fixed(List, Lengths, Char) ->
    case length(List) == length(Lengths) of
	true ->
	    join_to_fixed(List, Lengths, Char, []);
	false ->
	    {error, "different number of elements"}
    end.

%%%-------------------------------------------------------------------
%%% Fuction     : split_inclusive
%%% Description : split a string using a regexp as separator 
%%%               
%%% Starting from code found at 
%%% http://www.trapexit.org/Splitting_A_String
%%%
%%% example 
%%%   split("How about a nice   hawaiian punch?" " +").

split_inclusive(Str, Regex) ->
    {match, Matches} = re:matches(Str, Regex),
    regexp_loop(Str, [], 1, Matches).


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

join_to_fixed([H|List], [L|Lenghts], Char, Acc) ->
    join_to_fixed(List, Lenghts, Char, [string:left(H, L, Char)|Acc]);

join_to_fixed([], [], _Char, Acc) ->
     string:join(lists:reverse(Acc),"").
regexp_loop(Str, Parts, Index, []) ->
    lists:reverse([string:substr(Str, Index)] ++ Parts);

regexp_loop(Str, Parts, Index, Rem_Matches) ->
    {NextPt,PtLen} = hd(Rem_Matches),
    regexp_loop( Str, [ string:substr(Str, NextPt, PtLen),
                        string:substr(Str, Index, NextPt - Index)]
                      ++ Parts, NextPt + PtLen,
                      tl(Rem_Matches) ).

