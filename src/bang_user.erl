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
	% Params = yaws_api:parse_query(Arg),
	{ok, User} = yaws_api:queryvar(Arg, "user"),
	{ok, PW} = yaws_api:queryvar(Arg, "pw"),
	bang_db:getUser(User, bang_crypto:hash(PW)). 

createUser(Arg, _Path) ->
	{ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
	{ok, Uname} = rfc4627:get_field(Json, "username"),
	{ok, PW} = rfc4627:get_field(Json, "password"),
	Hash = bang_crypto:hash(PW), 
	bang_db:insertUser(binary_to_list(Uname), Hash).

updateUser(_Arg, _Path) ->
	{status, 501}.

deleteUser(_Arg, _Path) ->
	{status, 501}.