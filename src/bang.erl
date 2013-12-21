-module(bang).

-include("../include/yaws_api.hrl").
-export([out/1]).

apiVersionString() -> "1.0".
badRequestResponse() -> bang_utilities:simpleResponse("Oops. I don't see what you're looking for...", 404).
fourzerofour() -> bang_utilities:simpleResponse("Oops. I don't see what you're looking for...", 404).

%%application entry point
%%out/1 is called by yaws when it receives a request
out(Arg) ->
    error_logger:info_msg("Path info: ~p~n", [Arg#arg.pathinfo]),	
	Path = bang_utilities:path(Arg#arg.pathinfo),
	processBasePath(Path, Arg).


%%valid paths start with /api/1.0/
processBasePath([], _Arg) ->
	badRequestResponse();
processBasePath(Path, Arg) ->
	[Root | Rest] = Path,
	case string:to_lower(Root) of
		"api" ->
			processApiPath(Rest, Arg);
		_ ->
			badRequestResponse()
	end.

processApiPath([], _Arg) ->
	badRequestResponse();
processApiPath(Path, Arg) ->
	APIVersion = apiVersionString(),
	[Root | Rest] = Path,
	case string:to_lower(Root) of
		APIVersion ->
			processRequestPath(Rest, Arg);
		_ -> 
			badRequestResponse()
	end.

processRequestPath([], _Arg) ->
	badRequestResponse();
processRequestPath(Path, Arg) ->
			[Root | Rest] = Path, 
			case string:to_lower(Root) of 
				"" ->
					bang_utilities:simpleResponse("Hello", 200); 
				"form-register-enterprise" -> %user registration resource
					error_logger:info_msg("Processing user request...~n"),
			        bang_register:handle(Arg, Rest); 
			    "validation" -> %validation resource for post-registration
			    	error_logger:info_msg("Processing validation request...~n"),
			    	bang_validate:handle(Arg, Rest);			        
			    "form-sign-in" -> %sign-in resource
			    	error_logger:info_msg("Processing login request...~n"),
			    	bang_session_auth:handle(Arg, Rest);
			    "session-token" -> %session token
					error_logger:info_msg("Processing session token request...~n"),			    	
			    	bang_session_token:handle(Arg, Rest);
			    "user-profile" -> %retrieve user profile with valid session token
			    	error_logger:info_msg("Processing profile data request...~n"),
			    	bang_session_profile:handle(Arg, Rest);
			    "add-redirect" -> %not ready yet, may go away...
			    	error_logger:info_msg("Processing add redirect URI request...~n"),
			    	bang_session_auth:handle(Arg, Rest);
		        "teapot" -> %I love this status code:-)
		        	{status, 418};
		        "thc" -> %this one too...
		        	{status, 420};
		        _ -> %everything else...
		        	fourzerofour()
		    end.


