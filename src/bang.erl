-module(bang).

-include("../include/yaws_api.hrl").
-export([out/1]).

out(Arg) ->
    handle(Arg).

handle(Arg) ->
	Path = bang_utilities:path(Arg#arg.pathinfo),
	case Path of
		[] ->
			{status, 401};
		_ ->
			[Root | Rest] = Path, 
			case string:to_lower(Root) of 
				"user" ->
			        	bang_user:handle(Arg, Rest); 
			    "room" ->
			            bang_room:handle(Arg, Rest);
		        "message" ->
		                bang_message:handle(Arg, Rest);
		        "teapot" ->
		        	{status, 418};
		        "thc" ->
		        	{status, 420};
		        _ ->
		        	{status, 404}
		    end
	end.

