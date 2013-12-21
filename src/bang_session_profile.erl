-module(bang_session_profile).

-export([handle/2]).

handle(_Arg, _Path) ->
	ClientProfile = {obj, [{"favteam", <<"Chargers">>},
							{"favfood", <<"tacos">>},
							{"image_icon", <<":-)">>}]},
	error_logger:info_msg("Client profile: ~p~n", [ClientProfile]), 
	Data = {obj, [{"username", <<"abcd">>},
					{accttype, list_to_binary(integer_to_list(0))},
					{acct_stage, list_to_binary(integer_to_list(0))},
					{"email", <<"default@gmail.com">>},
					{"alias", <<"hijk alias">>},
					{"client_profile", ClientProfile}]},
	error_logger:info_msg("Data: ~p~n", [Data]),
	bang_utilities:jsonEncodedResponse(Data, 200). 