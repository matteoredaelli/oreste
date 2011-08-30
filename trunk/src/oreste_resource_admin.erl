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

-module(oreste_resource_admin).
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

-record(state, {auth=[], requests=0}).

init([Auth]) ->
    {ok, #state{auth=Auth}}.

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
    {Command, RQ} = parse_reqdata(ReqData, State),
    {ok, Result} = exec_admin_command(Command, RQ, State),
    NewState = State#state{requests = State#state.requests + 1},
    {Result, ReqData, NewState}.

%% Private Functions
exec_admin_command("help", _RQ, _State) ->
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
exec_admin_command("reload", ReqData, _State) ->
    %%RQ =  wrq:req_qs(ReqData),
    case wrq:get_qs_value("sql", ReqData) of
	undefined ->
	    Reply = "Missing sql parameter";
	Sql ->
	    SqlName = list_to_atom(Sql),
	    %% check if DSN exists
	    Children = oreste_util:which_children_names(oreste_sql_sup),
	    error_logger:info_msg("Checking if ~p is in ~p ~n", [SqlName, Children]),
	    case lists:member(SqlName, Children) of
		true ->
		    oreste_sql:reload_configuration(SqlName),
		    error_logger:info_msg("SQL file ~p reloaded! ~n", [SqlName]),
		    Reply = "Reloaded!";
		false ->
		    Reply = "NOT reloaded: missing name",
		    error_logger:info_msg("SQL file ~p not reloaded: missing process name ~n", [SqlName])
	    end
    end,
    {ok, Reply};
exec_admin_command("status", _ReqData, State) ->
    Children = lists:append(supervisor:which_children(oreste_dsn_sup),
			    supervisor:which_children(oreste_sql_sup)
			   ),
    StatusList = lists:map( fun({Name,_,_,[Module]}) -> Module:status(Name) end, Children),
    Reply = "Resource Requests=" ++ integer_to_list(State#state.requests) ++ 
	"\n\n" ++
	string:join(StatusList, "\n"),
    {ok, Reply};
exec_admin_command(Unknown,_ReqData,  _State) ->
    Reply = "Unknown command=" ++ Unknown,
    {ok, Reply}.

parse_reqdata(ReqData, _State) ->
    Params = wrq:path_info(ReqData),
    Command = dict:fetch(command, Params),
    {Command, ReqData}.

