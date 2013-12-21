-module(bang_invalidate).

-export([invalidateAuth/1]).%, invalidateSession/1]).

invalidateAuth(DocID) ->
	invalidateAuth(DocID, 0).
	

invalidateAuth(_DocID, 10) ->
	invalidate_too_many_retries;
invalidateAuth(DocID, ReqCount) ->
	URL = bang_private:couchSessionInvalidate() ++ "/" ++ DocID,
	error_logger:info_msg("Invalidating auth code at URL: ~p~n", [URL]), 
	processRequest(bang_http:put(URL, []), DocID, ReqCount).

processRequest({ok, {{_Version, 201, _ReasonPhrase}, _Headers, _Body}}, _DocID, _ReqCount) ->
	error_logger:info_msg("Auth code invalidate request code 201, Doc ID: ~p~n", [_DocID]), 
	ok;
processRequest({ok, {{_Version, 404, _ReasonPhrase}, _Headers, _Body}}, _DocID, _ReqCount) ->
	error_logger:info_msg("Auth code invalidate request failed with 404, Doc ID: ~p~n", [_DocID]), 	
	invalid_doc_id;
processRequest({ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, _Body}}, DocID, ReqCount) ->
	error_logger:info_msg("Auth code invalidate request code with ~d, Doc ID: ~p~n", [ResponseCode, DocID]), 
	timer:sleep(50), %% wait 50 milliseconds and try again
	invalidateAuth(DocID, ReqCount + 1);
processRequest(_Request, _DocID, _ReqCount) ->
	error_logger:info_msg("Auth code invalidate failed with error: ~p~n", [_Request]),
	invalidate_error.
 