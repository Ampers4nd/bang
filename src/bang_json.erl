-module(bang_json).

-export([encodedJson/1, contentType/0, contentHeader/0]).

contentType() -> "application/json".
contentHeader() -> {header, ["Content-Type:  ", contentType()]}.

encodedJson(Fields) ->
	encodedJson(Fields, []).

encodedJson([], Acc) ->  
	Record = {obj, lists:reverse(Acc)},
	rfc4627:encode(Record);
encodedJson(Fields, Acc) ->
	[{Key, Value} | Tail] = Fields,
	encodedJson(Tail, [{list_to_binary(Key), list_to_binary(Value)} | Acc]).