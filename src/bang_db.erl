-module(bang_db).
-export([doInsert/1, getUser/1]).

url() -> "http://localhost:5984/bang/".
% contentType() -> "application/json".

getRequest(UID) ->
	error_logger:info_msg("Doing get..."), 
	Header = [],
	HTTPOptions = [],
	Options = [],
	URL = url() ++ UID,
	error_logger:info_msg("URL: ~p~n", [URL]), 
	httpc:request(get, {URL, Header}, HTTPOptions, Options).

doInsert(Body) ->
	JSONEncodedBody = rfc4627:encode(Body),
	error_logger:info_msg("Body: ~p~n", [Body]), 
	{ok, UserType} = rfc4627:get_field(Body, "user_type"),
	error_logger:info_msg("Doing insert. JSON: ~p~n", [JSONEncodedBody]), 
	Request = bang_http:post(url(), JSONEncodedBody),
	error_logger:info_msg("Received response: ~p~n", [Request]),
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
					 bang_utilities:json_header(),
					 {status, ResponseCode}];
				_ ->
					error_logger:error_msg("DB post returned unexpected response code: ~p~n", [Request]),
					{status, ResponseCode}
				end;
		_ ->
			error_logger:error_msg("DB post failed: Request: ~p~n", [Request]) 
	end.

getUser(UID) ->
	error_logger:info_msg("Retrieving user..."), 
	Request = getRequest(UID),
	case Request of
		{ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, _Body}} ->
			{status, ResponseCode};
		_ ->
			{status, 400}
	end.