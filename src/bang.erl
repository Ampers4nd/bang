-module(bang).

-include("../include/yaws_api.hrl").
-export([out/1]).

apiVersionString() -> "1.0".
fourzerofour() -> bang_utilities:simpleResponse("Oops. I don't see what you're looking for...", 404).

%%application entry point
%%out/1 is called by yaws when it receives a request

%% http://stackoverflow.com/questions/20456578/routing-in-yaws
%%out(Arg) ->
    %% Uri = yaws_api:request_url(Arg),
    %% defined in yaws_api.hrl
    %% {url,"http","localhost",8000,"/",[]}
    %% Path = string:tokens(Uri#url.path, "/"),
%    Method = (Arg#arg.req)#http_request.method,
    %% error_logger:info_msg("Path info: port", Path),
    %% error_logger:info_msg("Path info: port", Uri#url.port),
    %% error_logger:info_msg("Path info: host", Uri#url.host),
    %% error_logger:info_msg("Path info: ----", Arg#arg.pathinfo),
    %% error_logger:info_msg("Path info: path", Uri#url.path).
%%    out(Arg, Method, Path).

out(Arg) ->
    error_logger:info_msg("Path info: ~p~n", [Arg#arg.pathinfo]),
    Path = bang_utilities:path(Arg#arg.pathinfo),
    processBasePath(Path, Arg).


%%valid paths start with /api/1.0/
processBasePath([], _Arg) ->
	fourzerofour();
processBasePath(Path, Arg) ->
	[Root | Rest] = Path,
	case string:to_lower(Root) of
		"api" ->
			processApiPath(Rest, Arg);
		_ ->
			fourzerofour()
	end.

processApiPath([], _Arg) ->
	fourzerofour();
processApiPath(Path, Arg) ->
	APIVersion = apiVersionString(),
	[Root | Rest] = Path,
	case string:to_lower(Root) of
		APIVersion ->
			processRequestPath(Rest, Arg);
		_ -> 
			fourzerofour()
	end.

processRequestPath([], _Arg) ->
	fourzerofour();
processRequestPath(Path, Arg) ->
			[Root | Rest] = Path, 
			case string:to_lower(Root) of 
				"hello" ->
					bang_utilities:simpleResponse("Hello", 200); 
				"register" -> %user registration resource
					error_logger:info_msg("Processing user request...~n"),
			        bang_register:handle(Arg, Rest); 
			    "validation" -> %post-registration validation resource
			    	error_logger:info_msg("Processing validation request...~n"),
			    	bang_validate:handle(Arg, Rest);			        
			    "sign-in" -> %sign-in resource
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


