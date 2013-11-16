-module(bang_utilities).

-export([path/1, method/1, headers/1, simpleResponse/2, jsonEncodedResponse/2, listContainsValue/2]).
-include("../include/yaws_api.hrl").

method(Arg) ->
    (Arg#arg.req)#http_request.method.

headers(Arg) ->
	Arg#arg.headers.

path(undefined) ->
	[];
path(Path) ->
    string:tokens(Path, "/").

simpleResponse(Message, ResponseCode) ->
	Record = {obj, [{"message", list_to_binary(Message)}]},
	[{html, rfc4627:encode(Record)},
	 bang_json:contentHeader(),
	 {status, ResponseCode}].

jsonEncodedResponse(Record, ResponseCode) ->
	[{html, rfc4627:encode(Record)},
	 bang_json:contentHeader(),
	 {status, ResponseCode}].

listContainsValue([], _Value) ->
	false;
listContainsValue(List, Value) ->
	[H|T] = List,
	case H  of
		Value ->
			true;
		_ ->
			listContainsValue(T, Value)
	end.


