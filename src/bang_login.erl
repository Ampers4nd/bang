-module(bang_login).

-include("../include/yaws_api.hrl").
-export([handle/2]).


handle(Arg, Path) ->
	case bang_utilities:method(Arg) of 
		'POST' -> 
			error_logger:info_msg("Received POST..."), 
			doAuth(Arg, Path);
		_ ->
			{status, 405}
	end.



%extract application_id, client_id, redirect_uri, credentials
%return auth_code wihth 200 if ok
%400 for missing args
%401 otherwise (incorrect or expired credentials)
doAuth(Arg, _Path) ->
	error_logger:info_msg("Received login request"),
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	error_logger:info_msg("login JSON: ~p~n", [JSON]),
	bang_login_db:doAuth(JSON).

