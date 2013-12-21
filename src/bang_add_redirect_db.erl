-module(bang_add_redirect_db).

-export([doUpdate/2]).

doUpdate([AppID, ClientID, UName, PW], _RedirectURIs) ->
	Parameters = yaws_api:url_encode("[\"" ++ AppID ++ "\",\"" 
                                        ++ ClientID ++ "\",\"" 
                                        ++ bang_crypto:sha512(UName) ++ "\",\"" 
                                        ++ bang_crypto:sha512(PW) ++"\"]"),
	QueryURL = bang_private:couchEnterpriseByCredentialsURL() ++ "?key=" ++ Parameters ++ "&include_docs=true",
	error_logger:info_msg("Query : ~p~n", [bang_http:get(QueryURL)]), 
	{status, 405}.