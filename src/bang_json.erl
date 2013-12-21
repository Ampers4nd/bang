-module(bang_json).

-export([simpleJsonObj/1, contentType/0, contentHeader/0, getField/2]).

contentType() -> "application/json".
contentHeader() -> {header, ["Content-Type:  ", contentType()]}.

%%argument is a list of tuples: [{Key1, Val1}, {Key2, Val2}, ...]
simpleJsonObj(Fields) ->
	simpleJsonObj(Fields, []).

simpleJsonObj([], Acc) ->  
	Record = {obj, lists:reverse(Acc)},
	rfc4627:encode(Record);
simpleJsonObj(Fields, Acc) ->
	[{Key, Value} | Tail] = Fields,
	simpleJsonObj(Tail, [{list_to_binary(Key), list_to_binary(Value)} | Acc]).

%%extract field from JSON object, 
%%generalizes rfc4627:get_field/2 to use a '.' separated key path
getField(EncodedJSON, KeyPath) ->
	getFieldTokenized(EncodedJSON, string:tokens(KeyPath, ".")).
 
getFieldTokenized(Value, []) ->
	{ok, Value};
getFieldTokenized(EncodedJSON, Keys) ->
	[Key, MoreKeys] = Keys,
	case rfc4627:get_field(EncodedJSON, Key) of
		{ok, Value} ->
			getFieldTokenized(Value, MoreKeys);
		_Oops ->
			{error, Key ++ " not found"}
	end.