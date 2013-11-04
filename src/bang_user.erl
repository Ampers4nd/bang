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
	error_logger:info_msg("User POST received...~n"),
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	processUserJSON(rfc4627:get_field(JSON, "username"), 
					rfc4627:get_field(JSON, "password"), 
					rfc4627:get_field(JSON, "data")).

processUserJSON({ok, UName}, {ok, PW}, {ok, Data}) ->
	error_logger:info_msg("Processing valid JSON...~n"), 
	EncryptedUName = bang_crypto:sha512(UName),
	EncryptedPW = bang_crypto:sha512(PW),
	Record = {obj, [{"user_type", <<"0">>}, %% user_type 0 -> not validated
					{"uname", list_to_binary(EncryptedUName)},
					{"pw", list_to_binary(EncryptedPW)},
	                {"data", Data}]},
	bang_db:doInsert(Record);
processUserJSON(_UName, _PW, _Data) ->
	error_logger:info_msg("There was a problem with the JSON~n"),
	Record = {obj, [{"message", <<"Missing or corrupted fields. :-(">>}]},
	[{html, rfc4627:encode(Record)},
		bang_json:contentHeader(),
		 {status, 400}].

updateUser(Arg, _Path) ->
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	bang_db:doUpdate(JSON).

deleteUser(_Arg, _Path) ->
	{status, 501}.