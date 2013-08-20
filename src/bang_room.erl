-module(bang_room).

-export([handle/2]).

handle(_Arg, _Path) ->
	{status, 501}.