-module(bang_user).

-include("../include/yaws_api.hrl").
-export([handle/2]).

handle(Arg, Path) ->
	error_logger:info_msg("X0"),
	case bang_utilities:method(Arg) of 
		'GET' ->
			getUser(Arg, Path);
		'POST' -> 
			createUser(Arg, Path);
		'PUT' ->
			updateUser(Arg, Path);
		'DELETE' ->
			deleteUser(Arg, Path);
		'HEAD' ->
			{status, 204};
		_ ->
			{status, 405}
	end.

getUser(Arg, _Path) ->
	{ok, UID} = yaws_api:queryvar(Arg, "uid"),
	bang_db:getUser(UID).

createUser(Arg, _Path) ->
	{ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
	bang_db:doInsert(Json).

updateUser(_Arg, _Path) ->
	{status, 501}.

deleteUser(_Arg, _Path) ->
	{status, 501}.