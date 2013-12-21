-module(bang_register_db).
-export([doInsert/4, doValidate/2]).

%%add new user
doInsert(EncryptedUName, EncryptedPW, UserType, Data) ->
	error_logger:info_msg("!!!!!!!!!!!!!!Doing insert:~nUname: ~p~n PW: ~p~n UserType: ~p~n Data: ~p~n!!!!!!!!!!!!!!!!!!!!", [EncryptedUName, EncryptedPW, UserType, Data]),
	Body = {obj, [{"uname", list_to_binary(EncryptedUName)},
					{"pw", list_to_binary(EncryptedPW)},
					{"user_type", list_to_binary(UserType)},
	                {"data", Data}]},
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
									{"user_type", list_to_binary(UserType)}]},
					bang_utilities:jsonEncodedResponse(Record, ResponseCode);
				_ ->
					error_logger:error_msg("DB post returned unexpected response code: ~p~n", [Request]),
					bang_utilities:simpleResponse("Insert failed", ResponseCode)
				end;
		_ ->
			error_logger:error_msg("DB post failed: Request: ~p~n", [Request]),
			bang_utilities:simpleResponse("Insert failed", 503)
	end.


%%validate user
%this is done by first retrieving the user by his UID, then updating the user_type parameter to some non-negative value
doValidate(UID, UserType) ->
	URL = bang_private:couchEnterpriseURL() ++ UID,
	error_logger:info_msg("Retrieving record from URL ~p~n", [URL]), 
	processUpdateGET(bang_http:get(URL), [UID, UserType]).

%%process GET response during update
processUpdateGET({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, EncodedJSONOld}}, [UID, UserType]) ->
	processUpdateGet(ResponseCode, EncodedJSONOld, [UID, UserType]);
processUpdateGET(_, _Params) ->
	bang_utilities:simpleResponse("Could not retrieve record", 503).

processUpdateGet(200, EncodedJSONOld, [UID, UserType]) ->
	error_logger:info_msg("Processing valid GET response, JSON: ~p~nUID: ~p~nUserType", [EncodedJSONOld, UID, UserType]), 
	{ok, DecodedJSONOld, _} = rfc4627:decode(list_to_binary(EncodedJSONOld)),
	{ok, CurrentUserType} = rfc4627:get_field(DecodedJSONOld, "user_type"), %skip validation of json coming from couch, b/c I put the data there...
	case CurrentUserType of 
		<<"0">> ->
			error_logger:info_msg("Replacing User type..."),
			JSONWithUserType = rfc4627:set_field(DecodedJSONOld, "user_type", list_to_binary(UserType)),
			error_logger:info_msg("User type replaced..."),
			AppID = bang_utilities:randomAppID(),
			ClientID =  bang_utilities:randomClientID(),
			error_logger:info_msg("AppID: ~p~nClientID: ~p~n", [AppID, ClientID]),
			JSONWithAppID = rfc4627:set_field(JSONWithUserType, "application_id", list_to_binary(AppID)),
			JSONWithClientID = rfc4627:set_field(JSONWithAppID, "client_id", list_to_binary(ClientID)),
			error_logger:info_msg("Final JSON: ~p~n", [JSONWithClientID]),
			URL = bang_private:couchEnterpriseURL() ++ UID,
			PUTRequest = bang_http:put(URL, rfc4627:encode(JSONWithClientID)),
			processUpdatePUT(PUTRequest, [UID, UserType, AppID, ClientID]);
		_ -> % user has already been validated
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
	error_logger:info_msg("JSON PUT Response: ~p~n", [JSONResponse]), 
	Record = {obj, [{"success", <<"true">>},
					{"user_type", list_to_binary(UserType)},
					{"application_id", list_to_binary(AppID)},
					{"client_id", list_to_binary(ClientID)}]}, 
	bang_utilities:jsonEncodedResponse(Record, 201); 
processUpdatePUT(ResponseCode, _ResponseBody, _Params) ->
	bang_utilities:simpleResponse("Update Failed", ResponseCode). 
