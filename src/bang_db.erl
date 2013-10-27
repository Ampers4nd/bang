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

doUpdate(Body) ->
	doUpdate(rfc4627:get_field(Body, "val_id"), rfc4627:get_field(Body, "user_type")).
	% {ok, UID} = rfc4627:get_field(Body, "val_id"),
	% {ok, UserType} = rfc4627:get_field(Body, "user_type"),
	% URL = baseURL() ++ binary_to_list(UID),
	% {ok, {{_Version, GETResponseCode, _ReasonPhrase}, _Headers, EncodedJSONOld}} = bang_http:get(URL), 
	% case GETResponseCode of
	% 	200 ->
	% 		{ok, DecodedJSONOld, _} = rfc4627:decode(list_to_binary(EncodedJSONOld)),
	% 		DecodedJSONNew = rfc4627:set_field(DecodedJSONOld, "user_type", UserType),
	% 		PUTRequest = bang_http:put(URL, rfc4627:encode(DecodedJSONNew)),
	% 		{ok, {{_Version2, PUTResponseCode, _ReasonPhrase2}, _Headers2, PUTResponseBody}} = PUTRequest,
	% 		case PUTResponseCode of
	% 			201 ->
	% 				{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(PUTResponseBody)),
	% 				error_logger:info_msg("JSON Response: ~p~n", [JSONResponse]), 
	% 				Record = {obj, [{"success", <<"true">>},
	% 								{"val_id", UID},
	% 								{"user_type", UserType}]},
	% 				Response = rfc4627:encode(Record),
	% 				[{html, Response},
	% 				 bang_json:contentHeader(),
	% 				 {status, PUTResponseCode}];
	% 			_ ->
	% 				{status, PUTResponseCode}
	% 		end;
	% 	_ ->
	% 		{status, GETResponseCode}
	% end.


doUpdate({ok, UID}, {ok, UserType}) ->
	URL = baseURL() ++ binary_to_list(UID),
	{ok, {{_Version, GETResponseCode, _ReasonPhrase}, _Headers, EncodedJSONOld}} = bang_http:get(URL), 
	case GETResponseCode of
		200 ->
			{ok, DecodedJSONOld, _} = rfc4627:decode(list_to_binary(EncodedJSONOld)),
			DecodedJSONNew = rfc4627:set_field(DecodedJSONOld, "user_type", UserType),
			PUTRequest = bang_http:put(URL, rfc4627:encode(DecodedJSONNew)),
			{ok, {{_Version2, PUTResponseCode, _ReasonPhrase2}, _Headers2, PUTResponseBody}} = PUTRequest,
			case PUTResponseCode of
				201 ->
					{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(PUTResponseBody)),
					error_logger:info_msg("JSON Response: ~p~n", [JSONResponse]), 
					Record = {obj, [{"success", <<"true">>},
									{"val_id", UID},
									{"user_type", UserType}]},
					Response = rfc4627:encode(Record),
					[{html, Response},
					 bang_json:contentHeader(),
					 {status, PUTResponseCode}];
				_ ->
					{status, PUTResponseCode}
			end;
		_ ->
			{status, GETResponseCode}
	end;
doUpdate(_, _) ->
	{status, 400}.
