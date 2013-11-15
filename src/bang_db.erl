-module(bang_db).
-export([doInsert/1, doUpdate/1, getUser/1]).

%%retrieve user
getUser(UID) ->
	error_logger:info_msg("Retrieving user..."), 
	processRetrieve(bang_http:get(bang_private:couchEnterpriseURL() ++ UID), UID).

processRetrieve({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, ResponseBody}}, UID) ->
	processRetrieve(UID, ResponseCode, ResponseBody);
processRetrieve(_, _UID) ->
	bang_utilities:simpleResponse("Failed to retrieve record", 503).

processRetrieve(200, UID, ResponseBody) ->
	{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(ResponseBody)),
	{ok, UserType} = rfc4627:get_field(JSONResponse, "user_type"),
	case UserType of
		<<"0">> ->
			bang_utilities:simpleResponse("USER_NOT_VALIDATED", 401);
		<<"-1">> ->
			bang_utilities:simpleResponse("USER_DELETED", 401); 
		_ ->
			{ok, Data} = rfc4627:get_field(JSONResponse, "data"),
			Record = {obj, [{"token", list_to_binary(UID)},
						{"user_type", UserType},
						{"data", Data}]},
			bang_utilities:jsonEncodedResponse(Record, 200)
	end;
processRetrieve(ResponseCode, _UID, _ResponseBody) ->
	bang_utilities:simpleResponse("Failed to retrieve record", ResponseCode).

%%add new user
doInsert(Body) ->
	error_logger:info_msg("!!!!!!!!!!!!!!Doing insert: ~p~n!!!!!!!!!!!!!!!!!!!!", [Body]),
	{ok, UserType} = rfc4627:get_field(Body, "user_type"),
	JSONEncodedBody = rfc4627:encode(Body),
	Request = bang_http:post(bang_private:couchEnterpriseURL(), JSONEncodedBody),
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
					bang_utilities:jsonEncodedResponse(Record, ResponseCode);
				_ ->
					error_logger:error_msg("DB post returned unexpected response code: ~p~n", [Request]),
					bang_utilities:simpleResponse("Insert failed", ResponseCode)
				end;
		_ ->
			error_logger:error_msg("DB post failed: Request: ~p~n", [Request]),
			bang_utilities:simpleResponse("Insert failed", 503)
	end.


%%update user
doUpdate(Body) ->
	error_logger:info_msg("!!!!!!!!!!!!!!Doing update: ~p~n!!!!!!!!!!!!!!!!!!!!~n", [Body]),
	doUpdate(rfc4627:get_field(Body, "token"), rfc4627:get_field(Body, "user_type")).

doUpdate({ok, UID}, {ok, UserType}) ->
	URL = bang_private:couchEnterpriseURL() ++ binary_to_list(UID),
	error_logger:info_msg("Retrieving record from URL ~p~n", [URL]), 
	processUpdateGET(bang_http:get(URL), [UID, UserType]);
doUpdate({ok, _UID}, _NotOK) ->
	bang_utilities:simpleResponse("Missing 'user_type'", 400);
doUpdate(_NotOK, {ok, _UserType}) ->
	bang_utilities:simpleResponse("Missing 'token'", 400);
doUpdate(_NotOK, _NotOK) ->
	bang_utilities:simpleResponse("Missing fields", 400). 

%%process GET response during update
processUpdateGET({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, EncodedJSONOld}}, [UID, UserType]) ->
	processUpdateGet(ResponseCode, EncodedJSONOld, [UID, UserType]);
processUpdateGET(_, _Params) ->
	bang_utilities:simpleResponse("Could not retrieve record", 503).

processUpdateGet(200, EncodedJSONOld, [UID, UserType]) ->
	{ok, DecodedJSONOld, _} = rfc4627:decode(list_to_binary(EncodedJSONOld)),
	{ok, CurrentUserType} = rfc4627:get_field(DecodedJSONOld, "user_type"),
	case CurrentUserType of 
		<<"0">> ->
			JSONWithUserType = rfc4627:set_field(DecodedJSONOld, "user_type", UserType),
			AppID = list_to_binary("AP_" ++ bang_crypto:randomString(13, 36)),
			ClientID =  list_to_binary("CL_" ++ bang_crypto:randomString(13, 36)),
			JSONWithAppID = rfc4627:set_field(JSONWithUserType, "application_id", AppID),
			JSONWithClientID = rfc4627:set_field(JSONWithAppID, "client_id", ClientID),
			URL = bang_private:couchEnterpriseURL() ++ binary_to_list(UID),
			PUTRequest = bang_http:put(URL, rfc4627:encode(JSONWithClientID)),
			processUpdatePUT(PUTRequest, [UID, UserType, AppID, ClientID]);
		_ ->
			bang_utilities:simpleResponse("You don't belong here.", 401)
	end;
processUpdateGet(ResponseCode, _JSON, _Params) ->
	bang_utilities:simpleResponse("Could not retrieve record", ResponseCode).

%%process PUT response during update
processUpdatePUT({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, ResponseBody}}, [UID, UserType, AppID, ClientID]) ->
	processUpdatePUT(ResponseCode, ResponseBody, [UID, UserType, AppID, ClientID]);
processUpdatePUT(_, _Params) ->
	bang_utilities:simpleResponse("Update Failed", 503).

processUpdatePUT(201, ResponseBody, [_UID, UserType, AppID, ClientID]) ->
	{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(ResponseBody)),
	error_logger:info_msg("JSON Response: ~p~n", [JSONResponse]), 
	Record = {obj, [{"success", <<"true">>},
					{"user_type", UserType},
					{"application_id", AppID},
					{"client_id", ClientID}]},
	bang_utilities:jsonEncodedResponse(Record, 201); 
processUpdatePUT(ResponseCode, _ResponseBody, _Params) ->
	bang_utilities:simpleResponse("Update Failed", ResponseCode). 