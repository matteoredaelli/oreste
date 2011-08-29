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

-module(oreste_resource).
-export(
   [
    content_types_provided/2,
%    encodings_provided/2,
%    generate_etag/2,
    init/1,
    is_authorized/2,
    to_text/2
   ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {auth=[], sqlName, requests=0}).

init([SqlName, Auth]) ->
    {ok, #state{auth=Auth, sqlName=list_to_atom(SqlName)}}.

content_types_provided(ReqData, State) ->
   {[{"text/plain",to_text}], ReqData, State}.

%%encodings_provided(Req, State) ->
%%  {[{"gzip", fun(X) -> zlib:gzip(X) end}], Req, State}.

%% expires(ReqData, Context) -> {{{2021,1,1},{0,0,0}}, ReqData, Context}.

%% generate_etag(ReqData, State) -> {wrq:raw_path(ReqData), ReqData, State}.

is_authorized(ReqData, State) ->
            case wrq:get_req_header("authorization", ReqData) of
                "Basic "++Base64 ->
                    Str = base64:mime_decode_to_string(Base64),
                    case State#state.auth of
                        Str ->
                            {true, ReqData, State};
                        _ ->
                            {"Basic realm=webmachine", ReqData, State}
                    end;
                _ ->
                    {"Basic realm=webmachine", ReqData, State}
            end.

to_text(ReqData, State) ->
    case parse_reqdata(ReqData, State) of
	{ok, admin, Command, _Extension} ->
	    {ok, Result} = exec_admin_command( Command, State);
        {ok, DSN, Command, Extension} ->
	    {_, Result} = exec_sql_command(DSN, Command, Extension, ReqData);
	{error, Result} ->
	    true
    end,
%    io:format("~ts", [Result]),
    NewState = State#state{requests = State#state.requests + 1},
    {Result, ReqData, NewState}.

%% Private Functions
exec_admin_command(help, _State) ->
    DSNout = "TODO",
    SQLout = "TODO",
	%% lists:foldl(
	%%   fun({Key,Value}, Acc) ->
	%% 	  case get_sql_parameters(Value) of
	%% 	      nomatch -> 
	%% 		  Params = "";
	%% 	      {match, ListParams} ->
	%% 		  Params = string:join(ListParams, ", ")
	%% 	  end,
	%% 	  Acc ++ "\n\t" ++ atom_to_list(Key) ++ ": " ++ Params end,
	%%   "SQL list:", 
	%%   State#state.sqlpool),
    {ok, DSNout ++ "\n" ++ SQLout};
exec_admin_command(reload, State) ->
    oreste_sql:reload_configuration(State#state.sqlName),
    Reply = "Configuration reload done!",
    {ok, Reply};
exec_admin_command(status, State) ->
    Children = lists:append(supervisor:which_children(oreste_dsn_sup),
			    supervisor:which_children(oreste_sql_sup)
			   ),
    StatusList = lists:map( fun({Name,_,_,[Module]}) -> Module:status(Name) end, Children),
    Reply = "Resource Requests=" ++ integer_to_list(State#state.requests) ++ 
	"\n\n" ++
	string:join(StatusList, "\n"),
    {ok, Reply}.

exec_sql_command(DSN, SQL, Extension, ReqData) ->
    io:format("Executing in DB ~s the SQL ~s~n", [DSN, SQL]),
    case Output = oreste_dsn:sql_query(DSN, SQL) of
	{error, Reason} ->
	    io_lib:format("Cannot run query to DB ~p. Reason:~p.", [DSN, Reason]);
	Output ->
	    case Extension of
		xml ->
		    odbc_output:to_xml(Output);
		csv ->
		    odbc_output:to_csv(Output, ",");
		xls ->
		    odbc_output:to_csv(Output, ";");
		txt ->
		    case wrq:get_qs_value("lengths",ReqData) of
			undefined ->
			    {error, "Missing lengths=N,M,.. parameter"};
			LengthsString ->
			    OptListOfStrings = string:tokens(LengthsString, ","),
			    Lengths = lists:map(
					fun(E) ->  list_to_integer(E) end,
					OptListOfStrings),
			    odbc_output:to_fixed(
			      Output, 
			      Lengths, 
			      $\  ) 
		    end;
		_Else ->
		    {error, "BUG: Unexpected extension"}
	    end
    end.


%% retreive all pamameters like {param1} inside a string
%% output: a list like ["param1", "param2, ...]

get_sql_parameters(SQL) ->
    case re:run(SQL,"{([[:alpha:]]+)}",[global,{capture, all, list} ]) of
	%% the output is like
	%% {match,[["{param}","param"],["{param}","param"]]}
	nomatch ->
	    nomatch;
	{match, List} ->
	    ResultList = lists:map(
	      fun(Param) -> lists:nth(2,Param) end,
	      List),
	    Result = sets:to_list( sets:from_list(ResultList) ),	
	    {match, Result}
    end.


parse_command("help", _ReqData, _State) ->
    {ok, help};
parse_command("reload", _ReqData, _State) ->
    {ok, reload};
parse_command("status", _ReqData, _State) ->
    {ok, status};
parse_command(Command, ReqData, State) when is_list(Command) ->
    CommandKey = list_to_atom(Command),
    SqlStatement = oreste_sql:get_sql_statement(State#state.sqlName, CommandKey),
    %% replacing params
    parse_sql_command(SqlStatement, ReqData).
	
parse_command_extension([Command, Extension], ReqData, State) ->
    case parse_extension(Extension) of 
	{error, Reason} ->
	    {error, Reason};
	{ok, Ext} ->
	    case parse_command(Command, ReqData, State) of		
		{error, Reason} ->
		    {error, Reason};
		{ok, SQL} ->
		    {ok, SQL, Ext}
	    end
    end;
parse_command_extension(_Else, _ReqData, _State ) ->
    {error, "wrong url: expected command.extension"}.

parse_dsn(admin, _State)->
    {ok, admin};
parse_dsn(DSNkey, _State) ->
    case lists:member(DSNkey, oreste_util:which_children_names(oreste_dsn_sup)) of
	false ->
	    Reason = io_lib:format("No valid DSN ~s.", [atom_to_list(DSNkey)]),
	    {error, Reason};
	true ->
	    {ok, DSNkey}
    end.

parse_extension(Ext)->
    case lists:member(Ext,["csv","xls","txt","xml"]) of
	true ->
	    {ok, list_to_atom(Ext)};
        false ->
	    Output = io_lib:format("Unexpected extension ~s.", [Ext]),
	    {error, Output}
    end.

parse_reqdata(ReqData, State) ->
    Params = wrq:path_info(ReqData),
    DSNkey = list_to_atom( dict:fetch(dsn, Params)),
    CommandExtension = dict:fetch(command, Params),

    case parse_command_extension(string:tokens(CommandExtension,"."), ReqData, State) of
	{error, M} ->
	    {error, M};
	{ok, Command, Extension} ->
	    case parse_dsn(DSNkey, State) of
		{error, Reason} ->
		    {error, Reason};
		{ok, DSN} ->
		    {ok, DSN, Command, Extension}
	    end
    end.


parse_sql_command(undefined, _ReqData) -> 
    {error, "Unknown SQL command"};
parse_sql_command(SQL, ReqData) ->
    % Now I retreive all url parameters and start replacing them in 
    % the SQL  statement. 
    % TODO "lengths" is dedicated to txt output and should be not used here
    RQ =  wrq:req_qs(ReqData),
    Result = lists:foldl(
	       fun({Key,Val}, Acc) ->
		       re:replace(
			 Acc, "{" ++ Key ++ "}",
			 Val,
			 [global,{return, list}]
			) end,
	       SQL, 
	       RQ
	      ),

    % are there any remaining parameters inside the
    % sql statement?

    case get_sql_parameters(Result) of
	nomatch ->
	    % all parameters {param} inside the sql statement were 
	    % correctly replaced!
	    {ok, Result};
	{match, ListParams} ->
	    Message = "Missing parameters:" ++ string:join(ListParams,","),
	    {error, Message}
    end.
