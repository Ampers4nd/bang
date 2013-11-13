-module(bang_session_db).

-export([doAuth/5, doToken/3]).

%%confirm credentials, create and return auth_code
doAuth(AppID, ClientID, _RedirectURI, EncryptedUName, EncryptedPW) ->
    Parameters = yaws_api:url_encode("[\"" ++ AppID ++ "\",\"" 
                                        ++ ClientID ++ "\",\"" 
                                        ++ EncryptedUName ++ "\",\"" 
                                        ++ EncryptedPW ++"\"]"), 
    QueryURL = bang_private:couchEnterpriseByCredentialsURL() ++ "?key=" ++ Parameters,
    processCredentialResponse(bang_http:get(QueryURL), AppID, ClientID).

processCredentialResponse({ok, {{_Version, 200, _ReasonPhrase}, _Headers, EncodedJSON}}, AppID, ClientID) -> 
    {ok, DecodedJSON, _} = rfc4627:decode(EncodedJSON),
    {ok, Rows} = rfc4627:get_field(DecodedJSON, "rows"),
    case Rows of
        [{obj, _Entries}] ->
            AuthCode = bang_crypto:randomBin(12, 36),
            {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
            error_logger:info_msg("TimeStamp: ~p~n", [{MegaSecs, Secs}]), 
            CreatedAt = MegaSecs * trunc(math:pow(10, 6)) + Secs,
            error_logger:info_msg("Created: ~p~n", [CreatedAt]), 
            Expires = CreatedAt + bang_config:authInterval(),
            error_logger:info_msg("Expires: ~p~n", [Expires]),             
            Record = {obj, [{"application_id", list_to_binary(AppID)},
                            {"client_id", list_to_binary(ClientID)},
                            {"auth_code", AuthCode},
                            {"created_at", integer_to_binary(CreatedAt)},
                            {"is_valid", <<"true">>},
                            {"expires", integer_to_binary(Expires)}]},
            error_logger:info_msg("Posting auth code to URL: ~p~nBody: ~p~n", [bang_private:couchSessionURL(), Record]), 
            Request = bang_http:post(bang_private:couchSessionURL(), rfc4627:encode(Record)),
            error_logger:info_msg("Auth code request: ~p~n", [Request]),
            processAuthCodePost(Request, AuthCode);
        [] ->
            bang_utilities:simpleResponse("Invalid credentials", 401)
    end;
processCredentialResponse({ok, {{_Version, _ResponseCode, _ReasonPhrase}, _Headers, _EncodedJSON}}, _AppID, _ClientID) ->
    bang_utilities:simpleResponse("Invalid credentials", 401);
processCredentialResponse(_, _AppID, _ClientID) ->
    bang_utilities:simpleResponse("Unable to process request", 503).


processAuthCodePost({ok, {{_Version, 201, _ReasonPhrase}, _Headers, _ResponseBody}}, AuthCode) ->
    Record = {obj, [{"success", <<"true">>},
                {"auth_code", AuthCode}]},
    bang_utilities:jsonEncodedResponse(Record, 200);
processAuthCodePost({ok, {{_Version, ResponseCode, ReasonPhrase}, _Headers, ResponseBody}}, _AuthCode) ->
    error_logger:info_msg("Auth code post failed with response code ~p~n, reason: ~p~n, body: ~p~n", [ResponseCode, ReasonPhrase, ResponseBody]),
    bang_utilities:simpleResponse("Login failed", 503);
processAuthCodePost(_, _AuthCode) ->
    errorLogger:info_msg("Auth code post failed~n"),
    bang_utilities:simpleResponse("Login failed", 500). 

%%exchange auth_code for token
doToken(AppID, ClientID, AuthCode) ->
    Parameters = yaws_api:url_encode("[\"" ++ AppID ++ "\",\"" 
                                        ++ ClientID ++ "\",\"" 
                                        ++ AuthCode ++ "\"]"),
    QueryURL = bang_private:couchSessionByAuthCode() ++ "?key=" ++ Parameters,
    processTokenGET(bang_http:get(QueryURL), AppID, ClientID).


processTokenGET({ok, {{_Version, 200, _ReasonPhrase}, _Headers, EncodedJSON}}, AppID, ClientID) ->
    {ok, DecodedJSON, _} = rfc4627:decode(EncodedJSON),
    {ok, Rows} = rfc4627:get_field(DecodedJSON, "rows"),
    case Rows of
        [] ->
            bang_utilities:simpleResponse("Invalid credentials", 401);
        [TheRow] ->
            {ok, RowValue} = rfc4627:get_field(TheRow, "value"),
            {ok, ExpiresBin} = rfc4627:get_field(RowValue, "expires"),
            Expires = binary_to_integer(ExpiresBin), 
            {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
            Now = MegaSecs * trunc(math:pow(10, 6)) + Secs,
            case ((Now - Expires) =< 0) of
                true ->
                    {ok, DocID} = rfc4627:get_field(TheRow, "id"),
                    spawn(bang_invalidate, invalidateAuth, [binary_to_list(DocID)]),
                    Token = bang_crypto:randomBin(12, 36),
                    TokenExpire = Now + bang_config:sessionInterval(),
                    Record = {obj, [{"application_id", list_to_binary(AppID)},
                                    {"client_id", list_to_binary(ClientID)},
                                    {"session_token", Token},
                                    {"created_at", integer_to_binary(Now)},
                                    {"is_valid", <<"true">>},
                                    {"expires", integer_to_binary(TokenExpire)}]},
                    TokenPostRequest = bang_http:post(bang_private:couchSessionURL(), rfc4627:encode(Record)),
                    processTokenPOST(TokenPostRequest, AppID, ClientID, Token); 
                false ->
                    bang_utilities:simpleResponse("Auth Code Expired", 401)
            end;
        _ ->
                bang_utilities:simpleResponse("Invalid credentials", 401)
    end.

processTokenPOST({ok, {{_Version, 201, _ReasonPhrase}, _Headers, _EncodedJSON}}, AppID, ClientID, Token) ->
    Record = {obj, [{"application_id", list_to_binary(AppID)},
                                    {"client_id", list_to_binary(ClientID)},
                                    {"session_token", Token}]},
    bang_utilities:jsonEncodedResponse(Record, 200); 
processTokenPOST({ok, {{_Version, _ResponseCode, _ReasonPhrase}, _Headers, _EncodedJSON}}, _AppID, _ClientID, _Token) ->
    error_logger:info_msg("Response Code: ~p~nReason: ~p~n", [_ResponseCode, _ReasonPhrase]), 
    bang_utilities:simpleResponse("Invalid credentials", 401);
processTokenPOST(_Request, _AppID, _ClientID, _Token) ->
    error_logger:info_msg("Request: ~p~n", [_Request]), 
    bang_utilities:simpleResponse("Unable to process request", 503).




