-module(bang_utilities).

-export([path/1, method/1]).
-include("../include/yaws_api.hrl").

method(Arg) ->
    (Arg#arg.req)#http_request.method.

path(undefined) ->
	[];
path(Path) ->
    string:tokens(Path, "/").