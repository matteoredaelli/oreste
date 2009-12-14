%%%-------------------------------------------------------------------
%%% File    : odbc_output.erl
%%% Author  : matteo <matteo.redaelli@libero.it>
%%% Description : 
%%%
%%% Created : 28 Aug 2009 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(odbc_output).

-define(CSV_FIELDS_SEPARATOR, ",").

%% API
-export([
	 to_csv/1,
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
to_csv({selected, Fields, Records}) ->
    Header = string:join(Fields, ?CSV_FIELDS_SEPARATOR),
    List = lists:map(
      fun record_to_csv_row/1,
      Records),
    {ok, Header ++ "\n" ++ string:join(List, "\n")};
%%
%% other outputs
%%
to_csv(Other) -> Other.
			   
%%--------------------------------------------------------------------
%% Function: to_xml
%% Description:

to_xml({selected, Keys, Records}) ->
    Indexes = lists:seq(1,length(Keys)), 
    Header = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n" ++ "<records>\n",
    ListOfRecords = lists:map(
      fun(Rec) ->
	      Values = list_to_list_of_strings( tuple_to_list(Rec) ),
	      List = lists:map(
		fun(I) ->
			Key = lists:nth(I, Keys),
			Value = lists:nth(I, Values),
			io_lib:format("    <~s>~s</~s>~n", [Key,Value,Key])
		end,
		Indexes),
	      "  <record>\n" ++ string:join(List,"") ++ "  </record>\n"
      end,
      Records),
    {ok, Header ++ string:join(ListOfRecords, "") ++ "</records>\n"};

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
	    {ok, string:join(List, "\n")};
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

record_to_csv_row(R) ->
    List = tuple_to_list(R),
    string:join(list_to_list_of_strings(List), ?CSV_FIELDS_SEPARATOR).

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
