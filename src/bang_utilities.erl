-module(bang_utilities).

-export([path/1, method/1, headers/1, simpleResponse/2, jsonEncodedResponse/2, listContainsValue/2, randomAppID/0, randomClientID/0, currentTimestamp/1]).
-include("../include/yaws_api.hrl").

method(Arg) ->
    (Arg#arg.req)#http_request.method.

headers(Arg) ->
	Arg#arg.headers.

path(undefined) ->
	[];
path(Path) ->
    string:tokens(Path, "/").

%%returns response with simple json object, {"message" : Message}
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

randomAppID() ->
	"AP_" ++ bang_crypto:randomString(13, 36).

randomClientID() ->
	"CL_" ++ bang_crypto:randomString(13, 36).

currentTimestamp(seconds) ->
	{MegaSecs, Secs, _MicroSecs} = os:timestamp(),
	MegaSecs * trunc(math:pow(10, 6)) + Secs;
currentTimestamp(milliseconds) ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	trunc(MegaSecs * math:pow(10, 9) + Secs * math:pow(10, 3) + MicroSecs/1000).

