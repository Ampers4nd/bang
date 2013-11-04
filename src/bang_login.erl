-module(bang_login).

-include("../include/yaws_api.hrl").
-export([handle/2]).


handle(Arg, Path) ->
	case bang_utilities:method(Arg) of 
		'GET' ->
			error_logger:info_msg("Received GET..."), 
			Record = {obj, {"hello", <<"hello">>}},
			Response = rfc4627:encode(Record),
			[{html, Response},
			 bang_json:contentHeader(),
			 {status, 200}];
		'POST' -> 
			error_logger:info_msg("Received GET..."), 
			doAuth(Arg, Path);
		'PUT' ->
			{status, 501};
		'DELETE' ->
			{status, 501};
		'HEAD' ->
			{status, 204};
		_ ->
			{status, 405}
	end.


doAuth(Arg, _Path) ->
	error_logger:info_msg("Received login request"),
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	error_logger:info_msg("login JSON: ~p~n", [JSON]),
	bang_login_db:doAuth(JSON).

