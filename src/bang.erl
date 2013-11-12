-module(bang).

-include("../include/yaws_api.hrl").
-export([out/1]).

apiVersionString() -> "1.0".
badRequestResponse() -> {status, 400}.

out(Arg) ->
    handle(Arg).

handle(Arg) ->
	error_logger:info_msg("!!!!Hello!!!!~n"),
    error_logger:info_msg("Path info: ~p~n", [Arg#arg.pathinfo]),	
	Path = bang_utilities:path(Arg#arg.pathinfo),
	processBasePath(Path, Arg).

processBasePath([], _Arg) ->
	badRequestResponse();
processBasePath(Path, Arg) ->
	error_logger:info_msg("X0~n"),
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
				"form-register-enterprise" ->
					error_logger:info_msg("Processing user request...~n"),
			        bang_user:handle(Arg, Rest); 
			    "validation" ->
			    	error_logger:info_msg("Processing validation request...~n"),
			    	bang_validation:handle(Arg, Rest);			        
			    "form-sign-in" ->
			    	error_logger:info_msg("Processing login request...~n"),
			    	bang_session_auth:handle(Arg, Rest);
			    "session-token" ->
					error_logger:info_msg("Processing session token request...~n"),			    	
			    	bang_session_token:handle(Arg, Rest);
		        "teapot" ->
		        	{status, 418};
		        "thc" ->
		        	{status, 420};
		        _ ->
		        	{status, 404}
		    end.


