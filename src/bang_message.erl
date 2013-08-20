-module(bang_message).

-export([handle/2]).

handle(_Arg, _Path) ->
	{status, 501}.