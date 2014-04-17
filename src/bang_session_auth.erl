-module(bang_session_auth).

-include("../include/yaws_api.hrl").
-export([handle/2]).

handle(Arg, Path) ->
	case bang_utilities:method(Arg) of 
		'POST' -> 
			error_logger:info_msg("Received POST..."), 
			doAuth(Arg, Path);
		_ ->
			{status, 405}
	end.

%extract application_id, client_id, redirect_uri, credentials
%return auth_code wihth 200 if ok
%400 for missing args
%401 otherwise (incorrect or expired credentials)
doAuth(Arg, _Path) ->
	error_logger:info_msg("Received login request"),
	DecodedBody = rfc4627:decode(Arg#arg.clidata),
	% error_logger:info_msg("Decoded JSON: ~p~n", [DecodedBody]), 
	case DecodedBody of
		{ok, JSON, _} ->
			error_logger:info_msg("login JSON: ~p~n", [JSON]),
			unpackCredentialsAndDoAuth(rfc4627:get_field(JSON, "application_id"), 
									   rfc4627:get_field(JSON, "client_id"), 
									   rfc4627:get_field(JSON, "redirect_uri"), 
									   rfc4627:get_field(JSON, "credentials"));
		_ ->
			bang_utilities:simpleResponse("Malformed POST Body", 400)
	end.
	
unpackCredentialsAndDoAuth({ok, AppID}, {ok, ClientID}, {ok, RedirectURI}, {ok, Credentials}) ->
	DecodedUName = rfc4627:get_field(Credentials, "username"),
	DecodedPW = rfc4627:get_field(Credentials, "password"),
	case {DecodedUName, DecodedPW} of 
		{{ok, UName}, {ok, PW}} ->
			error_logger:info_msg("Unpacked credentials..."), 
			bang_session_db:doAuth(binary_to_list(AppID), 
									binary_to_list(ClientID), 
									binary_to_list(RedirectURI), 
									bang_crypto:sha512(binary_to_list(UName)), 
									bang_crypto:sha512(binary_to_list(PW)));
		_ ->
			bang_utilities:simpleResponse("Credentials missing", 400)
	end;
unpackCredentialsAndDoAuth(_AppID, _ClientID, _RedirectURI, _Credentials) ->
	bang_utilities:simpleResponse("Missing Arguments in POST", 404). 