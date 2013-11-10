-module(bang_session_db).

-export([doAuth/5]).

doAuth(AppID, ClientID, _RedirectURI, EncryptedUName, EncryptedPW) ->
    Parameters = yaws_api:url_encode("[\"" ++ AppID ++ "\",\"" 
                                        ++ ClientID ++ "\",\"" 
                                        ++ EncryptedUName ++ "\",\"" 
                                        ++ EncryptedPW ++"\"]"), 
    QueryURL = bang_private:couchEnterpriseByCredentialsURL() ++ "?key=" ++ Parameters,
    error_logger:info_msg("Doing request with URL ~s~n", [QueryURL]), 
    processResponse(bang_http:get(QueryURL)).

processResponse({ok, {{_Version, 200, _ReasonPhrase}, _Headers, EncodedJSON}}) ->
    error_logger:info_msg("JSON: ~p~n", [EncodedJSON]), 
    {ok, DecodedJSON, _} = rfc4627:decode(EncodedJSON),
    error_logger:info_msg("Decoded JSON: ~p~n", [DecodedJSON]),
    error_logger:info_msg("~p~n", [rfc4627:get_field(DecodedJSON, "rows")]), 
    {ok, Rows} = rfc4627:get_field(DecodedJSON, "rows"),
    error_logger:info_msg("Rows: ~p~n", [Rows]), 
    case Rows of
        [{obj, _Entries}] ->
            AuthCode = bang_crypto:randomBin(12, 36),
            Record = {obj, [{"success", <<"true">>},
                            {"auth_code", AuthCode}]},
            bang_utilities:jsonEncodedResponse(Record, 200); 
        [] ->
            bang_utilities:simpleResponse("Invalid credentials", 401)
    end;
processResponse({ok, {{_Version, _ResponseCode, _ReasonPhrase}, _Headers, _EncodedJSON}}) ->
    bang_utilities:simpleResponse("Invalid credentials", 401);
processResponse(_) ->
    bang_utilities:simpleResponse("Unable to process request", 503).
