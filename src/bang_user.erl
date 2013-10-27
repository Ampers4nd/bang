-module(bang_user).

-include("../include/yaws_api.hrl").
-export([handle/2]).

handle(Arg, Path) ->
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
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	Record = {obj, [{"user_type", <<"0">>}, %% user_type 0 -> not validated
	                {"data", JSON}]},
	error_logger:info_msg("Record: ~p~n", [Record]),
	bang_db:doInsert(Record).

updateUser(Arg, _Path) ->
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	bang_db:doUpdate(JSON).

deleteUser(_Arg, _Path) ->
	{status, 501}.