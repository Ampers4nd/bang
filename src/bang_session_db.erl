-module(bang_session_db).

-export([doAuth/5]).

doAuth(AppID, ClientID, _RedirectURI, EncryptedUName, EncryptedPW) ->
    Parameters = yaws_api:url_encode("[\"" ++ AppID ++ "\",\"" 
                                        ++ ClientID ++ "\",\"" 
                                        ++ EncryptedUName ++ "\",\"" 
                                        ++ EncryptedPW ++"\"]"), 
    QueryURL = bang_private:couchEnterpriseByCredentialsURL() ++ "?key=" ++ Parameters,
    error_logger:info_msg("Doing request with URL ~s~n", [QueryURL]), 
    processCredentialResponse(bang_http:get(QueryURL)).

processCredentialResponse({ok, {{_Version, 200, _ReasonPhrase}, _Headers, EncodedJSON}}) ->
    error_logger:info_msg("JSON: ~p~n", [EncodedJSON]), 
    {ok, DecodedJSON, _} = rfc4627:decode(EncodedJSON),
    error_logger:info_msg("Decoded JSON: ~p~n", [DecodedJSON]),
    error_logger:info_msg("~p~n", [rfc4627:get_field(DecodedJSON, "rows")]), 
    {ok, Rows} = rfc4627:get_field(DecodedJSON, "rows"),
    error_logger:info_msg("Rows: ~p~n", [Rows]), 
    case Rows of
        [{obj, _Entries}] ->
            error_logger:info_msg("Rows look good:)~n"), 
            AuthCode = bang_crypto:randomBin(12, 36),
            error_logger:info_msg("Generated Auth Code: ~p~n", [AuthCode]), 
            Record = {obj, [{"auth_code", AuthCode}]},
            error_logger:info_msg("Posting auth code to URL: ~p~nBody: ~p~n", [bang_private:couchSessionURL(), Record]), 
            Request = bang_http:post(bang_private:couchSessionURL(), rfc4627:encode(Record)),
            error_logger:info_msg("Auth code request: ~p~n", [Request]),
            processAuthCodePost(Request, AuthCode);
        [] ->
            bang_utilities:simpleResponse("Invalid credentials", 401)
    end;
processCredentialResponse({ok, {{_Version, _ResponseCode, _ReasonPhrase}, _Headers, _EncodedJSON}}) ->
    bang_utilities:simpleResponse("Invalid credentials", 401);
processCredentialResponse(_) ->
    bang_utilities:simpleResponse("Unable to process request", 503).


processAuthCodePost({ok, {{_Version, 201, _ReasonPhrase}, _Headers, ResponseBody}}, AuthCode) ->
    error_logger:info_msg("Auth code added to db: ~p~n", [ResponseBody]), 
    Record = {obj, [{"success", <<"true">>},
                {"auth_code", AuthCode}]},
    bang_utilities:jsonEncodedResponse(Record, 200);
processAuthCodePost({ok, {{_Version, ResponseCode, ReasonPhrase}, _Headers, ResponseBody}}, _AuthCode) ->
    error_logger:info_msg("Auth code post failed with response code ~p~n, reason: ~p~n, body: ~p~n", [ResponseCode, ReasonPhrase, ResponseBody]),
    bang_utilities:simpleResponse("Login failed", 503);
processAuthCodePost(_, _AuthCode) ->
    errorLogger:info_msg("Auth code post failed~n"),
    bang_utilities:simpleResponse("Login failed", 500). 


