-module(bang_db).
-export([doInsert/1, doUpdate/1, getUser/1]).

baseURL() -> "http://localhost:5984/bang/".


%%retrieve user
getUser(UID) ->
	error_logger:info_msg("Retrieving user..."), 
	processRetrieve(bang_http:get(baseURL() ++ UID), UID).

processRetrieve({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, ResponseBody}}, UID) ->
	processRetrieve(UID, ResponseCode, ResponseBody);
processRetrieve(_, _UID) ->
	{status, 503}.

processRetrieve(200, UID, ResponseBody) ->
	{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(ResponseBody)),
	{ok, UserType} = rfc4627:get_field(JSONResponse, "user_type"),
	case UserType of
		<<"0">> ->
			Record = {obj, [{"message", <<"USER_NOT_VALIDATED">>}]},
			[{html, rfc4627:encode(Record)},
			 bang_json:contentHeader(),
		 	 {status, 401}];
		<<"-1">> ->
			Record = {obj, [{"message", <<"USER_DELETED">>}]},
			[{html, rfc4627:encode(Record)},
			 bang_json:contentHeader(),
		 	 {status, 401}];
		_ ->
			{ok, Data} = rfc4627:get_field(JSONResponse, "data"),
			Record = {obj, [{"token", list_to_binary(UID)},
						{"user_type", UserType},
						{"data", Data}]},
			[{html, rfc4627:encode(Record)},
			 bang_json:contentHeader(),
		 	 {status, 200}]
	end;
processRetrieve(ResponseCode, _UID, _ResponseBody) ->
	{status, ResponseCode}.

%%add new user
doInsert(Body) ->
	error_logger:info_msg("!!!!!!!!!!!!!!Doing insert: ~p~n!!!!!!!!!!!!!!!!!!!!", [Body]),
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
									{"token", UID},
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


%%update user
doUpdate(Body) ->
	error_logger:info_msg("!!!!!!!!!!!!!!Doing update: ~p~n!!!!!!!!!!!!!!!!!!!!", [Body]),
	doUpdate(rfc4627:get_field(Body, "token"), rfc4627:get_field(Body, "user_type")).

doUpdate({ok, UID}, {ok, UserType}) ->
	URL = baseURL() ++ binary_to_list(UID),
	processUpdateGET(bang_http:get(URL), [UID, UserType]);
doUpdate({ok, _UID}, _NotOK) ->
	Record = {obj, [{"message", <<"Missing 'user_type'">>}]},
	[{html, rfc4627:encode(Record)},
	 bang_json:contentHeader(),
	 {status, 400}];
doUpdate(_NotOK, {ok, _UserType}) ->
	Record = {obj, [{"message", <<"Missing 'token'">>}]},
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
	processUpdateGet(ResponseCode, EncodedJSONOld, [UID, UserType]);
processUpdateGET(_, _Params) ->
	{status, 503}.

processUpdateGet(200, EncodedJSONOld, [UID, UserType]) ->
	{ok, DecodedJSONOld, _} = rfc4627:decode(list_to_binary(EncodedJSONOld)),
	{ok, CurrentUserType} = rfc4627:get_field(DecodedJSONOld, "user_type"),
	case CurrentUserType of 
		<<"0">> ->
			JSONWithUserType = rfc4627:set_field(DecodedJSONOld, "user_type", UserType),
			JSONWithAppID = rfc4627:set_field(JSONWithUserType, "application_id", bang_crypto:randomBin(16, 36)),
			JSONWithClientID = rfc4627:set_field(JSONWithAppID, "client_id", bang_crypto:randomBin(16, 36)),
			URL = baseURL() ++ binary_to_list(UID),
			PUTRequest = bang_http:put(URL, rfc4627:encode(JSONWithClientID)),
			processUpdatePUT(PUTRequest, [UID, UserType]);
		_ ->
			Record = {obj, [{"message", <<"You don't belong here.">>}]},
			[{html, rfc4627:encode(Record)},
			 bang_json:contentHeader(),
			 {status, 401}]
	end;
processUpdateGet(ResponseCode, _JSON, _Params) ->
	{status, ResponseCode}.

%%process PUT response during update
processUpdatePUT({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, ResponseBody}}, [UID, UserType]) ->
	processUpdatePUT(ResponseCode, ResponseBody, [UID, UserType]);
processUpdatePUT(_, _Params) ->
	{status, 503}.

processUpdatePUT(201, ResponseBody, [UID, UserType]) ->
	{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(ResponseBody)),
	error_logger:info_msg("JSON Response: ~p~n", [JSONResponse]), 
	Record = {obj, [{"success", <<"true">>},
					{"token", UID},
					{"user_type", UserType}]},
	Response = rfc4627:encode(Record),
	[{html, Response},
	 bang_json:contentHeader(),
	 {status, 201}];
processUpdatePUT(ResponseCode, _ResponseBody, _Params) ->
	{status, ResponseCode}.