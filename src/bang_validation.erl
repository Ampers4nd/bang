-module(bang_validation).

-export([handle/2]).

handle(Arg, Path) ->		
	error_logger:info_msg("bang_val handle..."), 
	case bang_utilities:method(Arg) of 
		'GET' ->
			[Root | _Rest] = Path,
			doValidate(string:to_lower(Root));
		_ ->
			{status, 405}
	end.

doValidate(UID) ->
	UpdateRecord = {obj, [{"token", list_to_binary(UID)},
	                       {"user_type", <<"1">>}]},
	% Record = {obj, [{"success", <<"true">>},
	% 				{"auth_code", bang_crypto:randomBin(31, 36)}]},
	error_logger:info_msg("Attempting validation of UID ~p~n", [UID]),
	bang_db:doUpdate(UpdateRecord). 
	% [{html, rfc4627:encode(Record)},
	%  bang_json:contentHeader(),
	%  {status, 200}].