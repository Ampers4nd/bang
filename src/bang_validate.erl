-module(bang_validate).

-export([handle/2]).

handle(Arg, Path) ->		
	error_logger:info_msg("bang_val handle..."), 
	case bang_utilities:method(Arg) of 
		'GET' ->
			[Root | _Rest] = Path,
			doValidate(string:to_lower(Root));
		'HEAD' ->
			{status, 204};	
		_ ->
			{status, 405}
	end.

%%change user type to '1', valid users have non-negative user_type
doValidate(UID) ->
	UserType = "1",
	error_logger:info_msg("Attempting validation of UID ~p~n", [UID]),
	bang_register_db:doValidate(UID, UserType). 