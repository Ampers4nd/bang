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
	Something = "Ampersand",
	SomethingElse = "Question Mark",
	Record = {obj, [{"something", list_to_binary(Something)},
	{"somethingElse", list_to_binary(SomethingElse)}]},
	Response = rfc4627:encode(Record),
	[{status, 200},
	{header, ["Content-Type:  ", "application/json"]},
	{html, Response}].

create_user(Arg, _Path) ->
	Data = Arg#arg.clidata,
	io:format("Decoded JSON: ~p~n", [rfc4627:decode(Data)]),
	{status, 201}.

update_user(_Arg, _Path) ->
	{status, 501}.

delete_user(_Arg, _Path) ->
	{status, 501}.