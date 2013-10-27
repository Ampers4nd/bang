-module(bang_db).
-export([doInsert/1, doUpdate/1, getUser/1]).

baseURL() -> "http://localhost:5984/bang/".
% contentType() -> "application/json".

% getRequest(UID) ->
% 	error_logger:info_msg("Doing get..."), 
% 	Header = [],
% 	HTTPOptions = [],
% 	Options = [],
% 	URL = baseURL() ++ UID,
% 	error_logger:info_msg("URL: ~p~n", [URL]), 
% 	httpc:request(get, {URL, Header}, HTTPOptions, Options).

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
					 bang_utilities:json_header(),
					 {status, ResponseCode}];
				_ ->
					error_logger:error_msg("DB post returned unexpected response code: ~p~n", [Request]),
					{status, ResponseCode}
				end;
		_ ->
			error_logger:error_msg("DB post failed: Request: ~p~n", [Request]) 
	end.

% revID ([]) -> "";
% revID(Headers) ->
%     [{Key, Value}, Rest] = Headers,
%     case Key of
%     	"etag" ->
%     		Value;
%     	_ ->
%     		revID(Rest)
%    end.

doUpdate(Body) ->
	{ok, UID} = rfc4627:get_field(Body, "val_id"),
	{ok, UserType} = rfc4627:get_field(Body, "user_type"),
	URL = baseURL() ++ binary_to_list(UID),
	{ok, {{_Version, GETResponseCode, _ReasonPhrase}, _Headers, EncodedJSONOld}} = bang_http:get(URL),
	error_logger:info_msg("Update GET returned with status code ~p~n", [GETResponseCode]), 
	error_logger:info_msg("Retrieved JSON ~s~n", [EncodedJSONOld]), 
	case GETResponseCode of
		200 ->
			{ok, DecodedJSONOld, _} = rfc4627:decode(list_to_binary(EncodedJSONOld)),
			DecodedJSONNew = rfc4627:set_field(DecodedJSONOld, "user_type", UserType),
			error_logger:info_msg("Update PUT returned with status code ~p~n", DecodedJSONNew),
			PUTRequest = bang_http:put(URL, rfc4627:encode(DecodedJSONNew)),
			error_logger:info_msg("PUT Request ~p~n", [PUTRequest]), 
			{ok, {{_Version2, PUTResponseCode, _ReasonPhrase2}, _Headers2, PUTResponseBody}} = PUTRequest,
			error_logger:info_msg("Update PUT returned with status code ~p~n", [PUTResponseCode]), 
			case PUTResponseCode of
				201 ->
					{ok, JSONResponse, _} = rfc4627:decode(list_to_binary(PUTResponseBody)),
					error_logger:info_msg("JSON Response: ~p~n", [JSONResponse]), 
					Record = {obj, [{"success", <<"true">>},
									{"val_id", UID},
									{"user_type", UserType}]},
					Response = rfc4627:encode(Record),
					[{html, Response},
					 bang_utilities:json_header(),
					 {status, PUTResponseCode}];
				_ ->
					{status, PUTResponseCode}
			end;
		_ ->
			{status, GETResponseCode}
	end.
	% Request = bang_http:post(baseURL() ++ binary_to_list(UID), )


getUser(UID) ->
	error_logger:info_msg("Retrieving user..."), 
	Request = bang_http:get(baseURL() ++ UID),
	case Request of
		{ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, _Body}} ->
			{status, ResponseCode};
		_ ->
			{status, 400}
	end.