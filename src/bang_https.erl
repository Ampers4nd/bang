-module(bang_https).

-export([get/3, post/4]).

contentType(json) -> "application/json".

auth_header(User, PW) ->
    Encoded = base64:encode_to_string(lists:append([User,":",PW])),
    {"Authorization", "Basic " ++ Encoded}.

headers(User, PW) -> [auth_header(User, PW), {"Content-Type", contentType(json)}].
% sslHTTPOptions() -> [{essl, {verify, 0}}].
sslHTTPOptions() -> [].

get(User, PW, URL) ->
	request(get, User, PW, URL, []).

post(User, PW, URL, Body) ->
	request(post, User, PW, URL, Body).

request(get, User, PW, URL, _Body) ->
	Options = [],
	error_logger:info_msg("Doing GET request to URL: ~p~n", [URL]), 
	httpc:request(get, {URL, headers(User, PW)}, sslHTTPOptions(), Options);
request(post, User, PW, URL, Body) ->
	Options = [],
	Request = httpc:request(post, {URL, headers(User, PW), contentType(json), Body}, sslHTTPOptions(), Options),
	error_logger:info_msg("Request sent: ~p~n", [Request]).
