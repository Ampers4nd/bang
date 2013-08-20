-module(bang_user).

-export([handle/2]).

handle(Arg, Path) ->
	case bang_utilities:method(Arg) of 
		'GET' ->
			get_users(Arg, Path);
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

get_users(_Arg, _Path) ->
	{status, 501}.

create_user(_Arg, _Path) ->
	{status, 501}.

update_user(_Arg, _Path) ->
	{status, 501}.

delete_user(_Arg, _Path) ->
	{status, 501}.