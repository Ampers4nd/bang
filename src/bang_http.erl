-module(bang_http).

-export([get/1, get/3, head/1, put/2, post/2, post/4]).

auth_header(User, PW) ->
    Encoded = base64:encode_to_string(lists:append([User,":",PW])),
    {"Authorization", "Basic " ++ Encoded}.

headers(json) -> [{"Content-Type", bang_json:contentType()}].
headers(User, PW) -> [auth_header(User, PW)].
headers(User, PW, json) -> [auth_header(User, PW), {"Content-Type", bang_json:contentType()}].

% options() -> [{sync, true}].
options() -> [].
httpOptions() -> [].

get(URL) ->
	httpc:request(get, 
					{URL, []}, 
					httpOptions(),
					options()).

get(User, PW, URL) ->
	request(get, User, PW, URL, []).

post(URL, Body) ->
	httpc:request(post, 
					{URL, headers(json), bang_json:contentType(), Body},
		 			httpOptions(), 
		 			options()). 

post(User, PW, URL, Body) ->
	request(post, User, PW, URL, Body).

put(URL, Body) ->
	httpc:request(put, 
					{URL, headers(json), bang_json:contentType(), Body},
		 			httpOptions(), 
		 			options()). 


head(URL) ->
	httpc:request(head,
					{URL, []},
					[],
					[]).

request(get, User, PW, URL, _Body) ->
	httpc:request(get, 
					{URL, headers(User, PW)}, 
					httpOptions(), 
					options());
request(post, User, PW, URL, Body) ->
	httpc:request(post, 
					{URL, headers(User, PW, json), bang_json:contentType(), Body},
 					httpOptions(), 
 					options()).
