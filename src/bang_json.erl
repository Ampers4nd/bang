-module(bang_json).

-export([encodedJson/1]).

encodedJson(Fields) ->
	error_logger:info_msg("Received JSON request with fields: ~p~n", [Fields]),
	encodedJson(Fields, []).

encodedJson([], Acc) ->  
	Record = {obj, Acc},
	rfc4627:encode(Record);
encodedJson(Fields, Acc) ->
	[{Key, Value} | Tail] = Fields,
	encodedJson(Tail, [{list_to_binary(Key), list_to_binary(Value)} | Acc]).