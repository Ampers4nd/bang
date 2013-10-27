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
	case yaws_api:queryvar(Arg, "val_id") of
		{ok, UID} -> 
			bang_db:getUser(UID);
		_ ->
			{status, 404}
	end.

createUser(Arg, _Path) ->
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	Record = {obj, [{"user_type", <<"0">>}, %% user_type 0 -> not validated
	                {"data", JSON}]},
	bang_db:doInsert(Record).

updateUser(Arg, _Path) ->
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	bang_db:doUpdate(JSON).

deleteUser(_Arg, _Path) ->
	{status, 501}.