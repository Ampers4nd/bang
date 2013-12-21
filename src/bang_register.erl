-module(bang_register).

-include("../include/yaws_api.hrl").
-export([handle/2]).


handle(Arg, Path) ->
	case bang_utilities:method(Arg) of 
		'POST' -> %this resource supports user creation only
			createUser(Arg, Path); 		
		'HEAD' ->
			{status, 204};
		_ ->
			{status, 405}
	end.

createUser(Arg, _Path) ->
	error_logger:info_msg("User POST received...~n"),
	{ok, JSON, _} = rfc4627:decode(Arg#arg.clidata),
	processUserJSON(rfc4627:get_field(JSON, "username"), 
					rfc4627:get_field(JSON, "password"), 
					rfc4627:get_field(JSON, "data")).

processUserJSON({ok, UName}, {ok, PW}, {ok, Data}) ->
	error_logger:info_msg("Processing valid JSON...~nUname: ~p~n", [UName]), 
	EncryptedUName = bang_crypto:sha512(UName),
	EncryptedPW = bang_crypto:sha512(PW),
	UserType = "0",
	error_logger:info_msg("Insert Params: ~p~n", [EncryptedUName]),
	bang_register_db:doInsert(EncryptedUName, EncryptedPW, UserType, Data);
processUserJSON(_UName, _PW, _Data) ->
	error_logger:info_msg("Oops. Bad JSON~n"),
	Record = {obj, [{"message", <<"Missing or corrupted fields. :-(">>}]},
	[{html, rfc4627:encode(Record)},
		bang_json:contentHeader(),
		 {status, 400}].