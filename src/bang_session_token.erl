-module(bang_session_token).

-include("../include/yaws_api.hrl").
-export([handle/2]).

handle(Arg, Path) ->
	case bang_utilities:method(Arg) of 
		'POST' -> 
			error_logger:info_msg("Received token POST..."), 
			unpackJSON(Arg, Path);
		_ ->
			{status, 405}
	end.

%extract application_id, client_id, redirect_uri, credentials
%return auth_code wihth 200 if ok
%400 for missing args
%401 otherwise (incorrect or expired credentials)
unpackJSON(Arg, _Path) ->
	error_logger:info_msg("Received login request"),
	DecodedBody = rfc4627:decode(Arg#arg.clidata),
	case DecodedBody of
		{ok, JSON, _} ->
			error_logger:info_msg("login JSON: ~p~n", [JSON]),
			confirmCredentials(rfc4627:get_field(JSON, "application_id"), 
							   rfc4627:get_field(JSON, "client_id"), 
							   rfc4627:get_field(JSON, "auth_code"));
		_ ->
			bang_utilities:simpleResponse("Malformed POST Body", 400)
	end.

confirmCredentials({ok, AppID}, {ok, ClientID}, {ok, AuthCode}) ->
	bang_session_db:doToken(binary_to_list(AppID), 
							binary_to_list(ClientID), 
							binary_to_list(AuthCode));
confirmCredentials(_AppID, _ClientID, _AuthCode) ->
	bang_utilities:simpleResponse("Missing arguments", 400).
