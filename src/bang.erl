-module(bang).

-include("../include/yaws_api.hrl").
-export([out/1]).

out(Arg) ->
    handle(Arg).

handle(Arg) ->
	error_logger:info_msg("Entry point..."), 
	Path = bang_utilities:path(Arg#arg.pathinfo),
	case Path of
		[] ->
			{status, 401};
		_ ->
			[Root | Rest] = Path, 
			case string:to_lower(Root) of 
				"form-register-enterprise" ->
						error_logger:info_msg("Processing user request..."),
			        	bang_user:handle(Arg, Rest); 
			    "room" ->
			            bang_room:handle(Arg, Rest);
		        "message" ->
		                bang_message:handle(Arg, Rest);
		        "socket" ->
		        		bang_socket:handle(Arg, Rest);
		        "teapot" ->
		        	{status, 418};
		        "thc" ->
		        	{status, 420};
		        _ ->
		        	{status, 404}
		    end
	end.

