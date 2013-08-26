-module(bang_db).
-export([insertUser/2]).

dbConnection() ->
	case pgsql:connect("localhost", [{database, "bang"}]) of
		{ok, Conn} ->
			Conn;
		_ ->
			error
	end.

insertUser(Uname, Hash) ->
	Conn = dbConnection(),
	InsertQuery = insertQuery("users", ["uname", "hash"], [Uname, Hash]), 
	error_logger:info_msg("Insert Query: ~s~n", [InsertQuery]), 
	case pgsql:squery(Conn, InsertQuery) of
		{ok, Count} ->
			Record = {obj, [{"rows_inserted", integer_to_binary(Count)},
							{"success", list_to_binary("true")}]},
			Response = rfc4627:encode(Record),
			[{html, Response},
			bang_utilities:json_header(), 
			{status, 201}];
		{error, Error} ->
			error_logger:info_msg("~p:~p Insert failed: Insert Query: ~s~n Response:~p~n", [?MODULE, ?LINE, InsertQuery, Error]),
			[bang_utilities:json_header(), 
			{status, 500}]
	end.

insertQuery(Table, Columns, Values) ->
	"INSERT INTO " ++ Table ++ " (" ++ string:join(Columns, ", ") ++ ") " ++ " VALUES ('"  ++ string:join(Values, "', '") ++ "')".