-module(bang_db).
-export([insertUser/2, getUser/2]).

dbConnection() ->
	case pgsql:connect("localhost", [{database, "bang"}]) of
		{ok, Conn} ->
			Conn;
		_ ->
			error
	end.

insertUser(Uname, Hash) ->
	error_logger:info_msg("Inserting user..."), 
	Conn = dbConnection(),
	InsertQuery = insertQuery("users", ["uname", "hash"], [Uname, Hash]), 
	error_logger:info_msg("Insert Query: ~s~n", [InsertQuery]), 
	case pgsql:squery(Conn, InsertQuery) of
		{ok, Count} ->
			Record = {obj, [{"rows_inserted", integer_to_binary(Count)},
							{"success", <<"true">>}]},
			Response = rfc4627:encode(Record),
			[{html, Response},
			bang_utilities:json_header(), 
			{status, 200}];
		{error, Error} ->
			error_logger:info_msg("~p:~p Insert failed: Insert Query: ~s~n Response:~p~n", [?MODULE, ?LINE, InsertQuery, Error]),
			case Error of 
				{error, error, <<"23505">>, _, _} ->
					Record = {obj, [{"success", <<"false">>},
							{"message", <<"Username already exists">>}]},
							Response = rfc4627:encode(Record),
							[{html, Response},
							bang_utilities:json_header(), 
							{status, 200}];
				_ ->
					{status, 400}
			end
	end.

getUser(UnameToCheck, HashToCheck) ->
	Conn = dbConnection(),
	SelectQuery = selectUserQuery("users", ["uname"], UnameToCheck, HashToCheck),
	error_logger:info_msg("Select Query: ~s~n", [SelectQuery]),
	case pgsql:squery(Conn, SelectQuery) of
		{ok, _Columns, Rows} ->
			case length(Rows) of
				1 ->
					[{Uname}] = Rows,  %Uname is returned as a binary
					Record = {obj, [{"user", Uname},
							{"token", bang_crypto:randomString(64)},
							{"success", <<"true">>}]},
					Response = rfc4627:encode(Record),
					[{html, Response},
						{header, ["Content-Type:  ", "application/json"]},
						bang_utilities:json_header(),
						{status, 200}];
				0 ->
					Record = {obj, [{"message", <<"Invalid credentials">>},
							{"success", <<"false">>}]},
					Response = rfc4627:encode(Record),
					[{html, Response},
						bang_utiliites:json_header(),
						{status, 401}];
				_ ->
					Record = {obj, [{"message", <<"Ambiguous db response">>},
							{"success", <<"false">>}]},
					Response = rfc4627:encode(Record),
					[{html, Response},
						bang_utiliites:json_header(),
						{status, 500}]
			end;
		_ ->
			Record = {obj, [{"message", <<"DB access failed">>},
							{"success", <<"false">>}]},
			Response = rfc4627:encode(Record),
			[{html, Response},
				bang_utiliites:json_header(),
				{status, 500}]
	end.


insertQuery(Table, Columns, Values) ->
	"INSERT INTO " ++ Table ++ " (" ++ string:join(Columns, ", ") ++ ") " ++ " VALUES ('"  ++ string:join(Values, "', '") ++ "')".

selectUserQuery(Table, Columns, Uname, Hash) ->
	"SELECT " ++ string:join(Columns, ", ") ++ " FROM " ++ Table ++ " WHERE uname LIKE '" ++ Uname ++ "' AND pw_hash LIKE '" ++ Hash ++ "'". 
