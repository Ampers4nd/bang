-module(bang_user).

-include("../include/yaws_api.hrl").
-export([handle/2]).

handle(Arg, Path) ->
	case bang_utilities:method(Arg) of 
		'GET' ->
			users(Arg, Path);
		'POST' -> 
			create_user(Arg, Path);
		'PUT' ->
			update_user(Arg, Path);
		'DELETE' ->
			delete_user(Arg, Path);
		'HEAD' ->
			{status, 204};
		_ ->
			{status, 405}
	end.

users(_Arg, _Path) ->
	Something = "something",
	SomethingElse = "something else",
	Record = {obj, [{"something", list_to_binary(Something)},
					{"somethingElse", list_to_binary(SomethingElse)}]},
	Response = rfc4627:encode(Record),
	[{status, 200},
	{header, ["Content-Type:  ", "application/json"]},
	{html, Response}].

create_user(Arg, _Path) ->
	{ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
	{ok, Uname} = rfc4627:get_field(Json, "username"),
	{ok, PW} = rfc4627:get_field(Json, "password"),
	Hash = bang_crypto:sha512(PW), 
	error_logger:info_msg("PW Hash: ~s~n", [Hash]), 
	bang_db:insertUser(binary_to_list(Uname), Hash).

update_user(_Arg, _Path) ->
	{status, 501}.

delete_user(_Arg, _Path) ->
	{status, 501}.