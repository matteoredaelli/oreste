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

-module(odbc_output).

%% API
-export([
	 to_csv/2,
	 to_fixed/3,
	 to_xml/1
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: to_csv
%% Description:
%%
%% odbc:sql_query(Ref, "SELECT * FROM EMPLOYEE").
%%    {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],
%%          [{1,"Jane","Doe","F"},
%%           {2,"John","Doe","M"},
%%           {3,"Monica","Geller","F"},
%%           {4,"Ross","Geller","M"},
%%           {5,"Rachel","Green","F"},
%%           {6,"Piper","Halliwell","F"},
%%           {7,"Prue","Halliwell","F"},
%%           {8,"Louise","Lane","F"}]]} 
%% B = {selected,["NR","FIRSTNAME","LASTNAME","GENDER"],[{1,"Jane","Doe","F"}, {2,"John","Doe","M"}, {3,"Monica","Geller","F"} ]}.
%%--------------------------------------------------------------------

%%
%% output of sql_query (at least one record)
%%
to_csv({selected, Fields, Records}, FieldSeparator) ->
    Header = string:join(Fields, FieldSeparator),
    List = lists:map(
	     fun(Record) ->
		     record_to_csv_row(Record, FieldSeparator)
	     end,
	     Records),
    Body = string:join(List, "\n"),
    Result = io_lib:format("~s~n~s", [Header, Body]),
    {ok, Result};
%%
%% other outputs
%%
to_csv(Other, _) -> Other.
			   
%%--------------------------------------------------------------------
%% Function: to_xml
%% Description:

to_xml({selected, Keys, Records}) ->
    Indexes = lists:seq(1,length(Keys)), 
    Header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ "<records>\n",
    Footer = "</records>\n",
    ListOfRecords = lists:map(
      fun(Rec) ->
	      Values = list_to_list_of_strings( tuple_to_list(Rec) ),
	      List = lists:map(
		fun(I) ->
			Key = lists:nth(I, Keys),
			Value = lists:nth(I, Values),
			io_lib:format("    <~s><![CDATA[~s]]></~s>~n", [Key,Value,Key])
		end,
		Indexes),
	      "  <record>\n" ++ string:join(List,"") ++ "  </record>\n"
      end,
      Records),
    {ok, Header ++ string:join(ListOfRecords, "") ++ Footer};

to_xml(Other) -> Other.

%%--------------------------------------------------------------------
%% Function: to_fixed
%% Description: output to a fixed size text file
%% 
%% Input:
%%   1: output of sql_query
%%   2: list of integers: the length for each column
%%   3: the character used to fill empty spaces
%% 
%% output of sql_query (at least one record)
%%
to_fixed({selected, Fields, Records}, Lengths, Char) ->
    case length(Fields) == length(Lengths) of
	true ->
	    List = lists:map(
		     fun(R) -> record_to_fixed_row(R,Lengths, Char) end,
		     Records),
	    Body = string:join(List, "\n"),
	    {ok, Body};
	false ->
	    {error, "Different number of columns"}
    end;
%%
%% other outputs
%%
to_fixed(Other, _Lengths, _Char) -> Other.


%%====================================================================
%% Internal functions
%%====================================================================

record_to_csv_row(R, FieldSeparator) ->
    List = tuple_to_list(R),
    string:join(list_to_list_of_strings(List), FieldSeparator).

record_to_fixed_row(R, Lengths, Char) ->
    List = tuple_to_list(R),
    emisc_string:join_to_fixed(list_to_list_of_strings(List), Lengths, Char ).

list_to_list_of_strings(List) ->
    lists:map(
      fun(V) -> 
	      if is_integer(V) ->
		      integer_to_list(V); 
		 is_float(V) ->
		      float_to_list(V);
		 null == V ->
		      "";
		 true -> V
	      end
      end, List).
