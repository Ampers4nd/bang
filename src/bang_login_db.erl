-module(bang_login_db).

-export([doAuth/1]).

doAuth(_JSON) ->	
	{status, 200}.

	% error_logger:info_msg("Sending response..."), 
	% {ok, AppID} = rfc4627:get_field(Body, "application_id"),
 %    {ok, ClientID} = rfc4627:get_field(Body, "client_id"),
 %    {ok, Credentials} = rfc4627:get_field(Body, "credentials"),
 %    EncodedCredentials = rfc26
 %    {ok, Uname} = rfc4627:get_field(Credentials, "username"),
 %    {ok, Password} = rfc4627:get_field(Credentials, "password"),	
	% {status, 200}.
    % {ok, AppID} = rfc4627:get_field(Body, "application_id"),
    % {ok, ClientID} = rfc4627:get_field(Body, "client_id"),
    % {ok, Credentials} = rfc4627:get_field(Body, "credentials"),
    % EncodedCredentials = rfc26
    % {ok, Uname} = rfc4627:get_field(Credentials, "username"),
    % {ok, Password} = rfc4627:get_field(Credentials, "password"),


