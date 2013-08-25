-module(bang_utilities).

-export([path/1, method/1, dbConnection/0, insert_query/3, json_header/0]).
-include("../include/yaws_api.hrl").

method(Arg) ->
    (Arg#arg.req)#http_request.method.

path(undefined) ->
	[];
path(Path) ->
    string:tokens(Path, "/").

dbConnection() ->
	case pgsql:connect("localhost", [{database, "bang"}]) of
		{ok, Conn} ->
			Conn;
		_ ->
			error
	end.

insert_query(Table, Columns, Values) ->
	"INSERT INTO " ++ Table ++ " (" ++ string:join(Columns, ", ") ++ ") " ++ " VALUES ('"  ++ string:join(Values, "', '") ++ "')".

json_header()->
	{header, ["Content-Type:  ", "application/json"]}.