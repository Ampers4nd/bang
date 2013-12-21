-module(bang_add_redirect).

-include("../include/yaws_api.hrl").
-export([handle/2]).

handle(Arg, _Path) ->
	case bang_utilities:method(Arg) of 
		'POST' -> 
			error_logger:info_msg("Add-redirect POST..."), 
			doUpdate(Arg);
		_ ->
			{status, 405}
	end.

doUpdate(Arg) ->
	DecodedBody = rfc4627:decode(Arg#arg.clidata),
	case DecodedBody of
		{ok, EncodedJSON, _} ->
			error_logger:info_msg("login JSON: ~p~n", [EncodedJSON]),
			case unpackedJSON(EncodedJSON) of
				{ok, Credentials, RedirectURIs} ->
					bang_add_redirect_db:doUpdate(Credentials, RedirectURIs);
				{error, Message} ->
					bang_utilities:simpleResponse(Message, 400)
			end;
		_ ->
			bang_utilities:simpleResponse("Malformed POST Body", 400)
	end.

unpackedJSON(EncodedJSON) ->
	unpackedJSON(bang_utilities:getField(EncodedJSON, "application_id"),
				bang_utilities:getField(EncodedJSON, "client_id"),
				bang_utilities:getField(EncodedJSON, "credentials.username"),
				bang_utilities:getField(EncodedJSON, "credentials.password"),
									bang_utilities:getField(EncodedJSON, "redirect_uris")).

unpackedJSON({ok, AppID}, {ok, ClientID}, {ok, UName}, {ok, PW}, {ok, RedirectURIs}) ->
	{ok, [AppID, ClientID, UName, PW], RedirectURIs};
unpackedJSON(_AppID, _ClientID, _UName, _PW, _RedirectURIS) ->
	{error, "Invalid or missing Arguments"}.

