-module(bang_db).
-export([doInsert/1, doUpdate/1, getUser/1]).

baseURL() -> "http://localhost:5984/bang/".

getUser(UID) ->
	error_logger:info_msg("Retrieving user..."), 
	Request = bang_http:get(baseURL() ++ UID),
	case Request of
		{ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, ResponseBody}} ->
			case ResponseCode of
				200 ->
					{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(ResponseBody)),
					{ok, UserType} = rfc4627:get_field(JSONResponse, "user_type"),
					{ok, Data} = rfc4627:get_field(JSONResponse, "data"),
					Record = {obj, [{"val_id", list_to_binary(UID)},
									{"user_type", UserType},
									{"data", Data}]},
					[{html, rfc4627:encode(Record)},
					 bang_json:contentHeader(),
					 {status, ResponseCode}];									
				_ ->
					{status, ResponseCode}
			end;
		_ ->
			{status, 503}
	end.

doInsert(Body) ->
	{ok, UserType} = rfc4627:get_field(Body, "user_type"),
	JSONEncodedBody = rfc4627:encode(Body),
	Request = bang_http:post(baseURL(), JSONEncodedBody),
	case Request of
		{ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, ResponseBody}} ->
			case ResponseCode of
				201 ->
					{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(ResponseBody)),
					error_logger:info_msg("JSON Response: ~p~n", [JSONResponse]), 
					{ok, UID} = rfc4627:get_field(JSONResponse, "id"),
					Record = {obj, [{"success", <<"true">>},
									{"val_id", UID},
									{"user_type", UserType}]},
					Response = rfc4627:encode(Record),
					[{html, Response},
					 bang_json:contentHeader(),
					 {status, ResponseCode}];
				_ ->
					error_logger:error_msg("DB post returned unexpected response code: ~p~n", [Request]),
					{status, ResponseCode}
				end;
		_ ->
			error_logger:error_msg("DB post failed: Request: ~p~n", [Request]),
			{status, 503}
	end.


%%update
doUpdate(Body) ->
	error_logger:info_msg("Doing Update..."), 
	doUpdate(rfc4627:get_field(Body, "val_id"), rfc4627:get_field(Body, "user_type")).

doUpdate({ok, UID}, {ok, UserType}) ->
	error_logger:info_msg("UID: ~p, UserType ~p~n", [UID, UserType]), 
	URL = baseURL() ++ binary_to_list(UID),
	processUpdateGET(bang_http:get(URL), [UID, UserType]);
doUpdate({ok, _UID}, _NotOK) ->
	Record = {obj, [{"message", <<"Missing 'user_type'">>}]},
	[{html, rfc4627:encode(Record)},
	 bang_json:contentHeader(),
	 {status, 400}];
doUpdate(_NotOK, {ok, _UserType}) ->
	Record = {obj, [{"message", <<"Missing 'val_id'">>}]},
	[{html, rfc4627:encode(Record)},
	bang_json:contentHeader(),
	{status, 400}];
doUpdate(_NotOK, _NotOK) ->
	Record = {obj, [{"message", <<"Missing fields">>}]},
	[{html, rfc4627:encode(Record)},
	bang_json:contentHeader(),
	{status, 400}].

%%process GET response during update
processUpdateGET({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, EncodedJSONOld}}, [UID, UserType]) ->
	error_logger:info_msg("Received GET response ~p~n", ResponseCode), 
	processUpdateGet(ResponseCode, EncodedJSONOld, [UID, UserType]);
processUpdateGET(_, _Params) ->
	{status, 503}.

processUpdateGet(200, EncodedJSONOld, [UID, UserType]) ->
	error_logger:info_msg("Processing GET 200: ~p~nUID: ~p, UserType: ~p~n", [EncodedJSONOld, UID, UserType]),
	{ok, DecodedJSONOld, _} = rfc4627:decode(list_to_binary(EncodedJSONOld)),
	error_logger:info_msg("Old Decoded JSON: ~p~n", [DecodedJSONOld]),
	DecodedJSONNew = rfc4627:set_field(DecodedJSONOld, "user_type", UserType),
	error_logger:info_msg("New Decoded JSON: ~p~n", [DecodedJSONNew]),
	URL = baseURL() ++ binary_to_list(UID),
	error_logger:info_msg("URL: ~p~n", [URL]),
	PUTRequest = bang_http:put(URL, rfc4627:encode(DecodedJSONNew)),
	error_logger:info_msg("PUT Request: ~p~n", [PUTRequest]),
	processUpdatePUT(PUTRequest, [UID, UserType]);
processUpdateGet(ResponseCode, _JSON, _Params) ->
	{status, ResponseCode}.

%%process PUT response during update
processUpdatePUT({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, ResponseBody}}, [UID, UserType]) ->
	error_logger:info_msg("Processing update PUT...~n"),
	processUpdatePUT(ResponseCode, ResponseBody, [UID, UserType]);
processUpdatePUT(_, _Params) ->
	{status, 503}.

processUpdatePUT(201, ResponseBody, [UID, UserType]) ->
	{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(ResponseBody)),
	error_logger:info_msg("JSON Response: ~p~n", [JSONResponse]), 
	Record = {obj, [{"success", <<"true">>},
					{"val_id", UID},
					{"user_type", UserType}]},
	Response = rfc4627:encode(Record),
	[{html, Response},
	 bang_json:contentHeader(),
	 {status, 201}];
processUpdatePUT(ResponseCode, _ResponseBody, _Params) ->
	{status, ResponseCode}.