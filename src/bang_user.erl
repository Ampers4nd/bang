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
	Params = yaws_api:parse_query(Arg),
	error_logger:info_msg("~p~p User parameters~n", [?MODULE, ?LINE, Params]) ,
	{ok, User} = yaws_api:queryvar(Arg, "user"),
	error_logger:info_msg("~p~p User: ~s~n", [?MODULE, ?LINE, User]), 	
	{ok, PW} = yaws_api:queryvar(Arg, "pw"),
	error_logger:info_msg("~p~p PW: ~s~n", [?MODULE, ?LINE, PW]), 		
	bang_db:getUser(User, bang_crypto:hash(PW)). 

createUser(Arg, _Path) ->
	{ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
	{ok, Uname} = rfc4627:get_field(Json, "username"),
	{ok, PW} = rfc4627:get_field(Json, "password"),
	Hash = bang_crypto:hash(PW), 
	error_logger:info_msg("PW Hash: ~s~n", [Hash]), 
	bang_db:insertUser(binary_to_list(Uname), Hash).

updateUser(_Arg, _Path) ->
	{status, 501}.

deleteUser(_Arg, _Path) ->
	{status, 501}.