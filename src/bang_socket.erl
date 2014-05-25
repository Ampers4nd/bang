-module(bang_socket).

-include("../include/yaws_api.hrl").
-export([handle/2, isSocketRequest/1, handle_message/1, say_hi/1]).

handle(Arg, _Path) ->
    error_logger:info_msg("Received socket request. Checking header"),
    case isSocketRequest(Arg#arg.headers) of
        true ->
            error_logger:info_msg("Received websocket request"), 
            [{status, 101}, 
             {websocket, bang_socket, []}];
        false ->
            Record = {obj, [{"error", <<"Socket request missing upgrade header">>}]},
            Response = rfc4627:encode(Record),
            [{html, Response},
             bang_json:contentHeader(),
             {status, 400}]
    end.

isSocketRequest(#headers{other=L}) ->
	error_logger:info_msg("Chekcing request~n"),
	lists:foldl(fun({http_header, _, K0, _, _V}, false) ->
		K = case is_atom(K0) of
			true ->
				atom_to_list(K0);
			false ->
				K0
			end,
		case string:to_lower(K) of
			"upgrade" ->
				true;
			_ ->
				false
			end;
		(_, Acc) ->
			Acc
		end, false, L).

handle_message({text, Message}) ->
	error_logger:info_msg("~p:~p received message on socket: ~p~n", [?MODULE, ?LINE, Message]),
	{reply, {text, <<Message/binary>>}}. 

say_hi(Pid) ->
	error_logger:info_msg("Saying hello, async style~n"), 
	yaws_api:websocket_send(Pid, {text, <<"Hola!">>}).
