-module(bang_crypto).

-export([hash/1, sha1/1, sha256/1, sha512/1, randomString/1, randomString/2, randomBin/1, randomBin/2]).

%%%
% All functions accept and return strings (lists)!
%%%

hash(Data) ->
	sha512(Data).

sha1(Data) ->
	hexstring(sha1, crypto:hash(sha, Data)).

sha256(Data) ->
	hexstring(sha256, crypto:hash(sha256, Data)). 

sha512(Data) ->
	error_logger:info_msg("Doing sha512..."),
	hexstring(sha512, crypto:hash(sha512, Data)). 

hexstring(sha1, Binary) ->
	hexstring160(Binary);
hexstring(sha256, Binary) ->
	hexstring256(Binary);
hexstring(sha512, Binary) ->
	hexstring512(Binary).		

%%i took the unpacking stuff from here: http://www.enchantedage.com/hex-format-hash-for-md5-sha1-sha256-and-sha512
hexstring160(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X])).

hexstring256(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X])).	

hexstring512(<<X:512/big-unsigned-integer>>) ->
	error_logger:info_msg("Unpacking response..."),	
    lists:flatten(io_lib:format("~128.16.0b", [X])).	        

randomString(Length) ->
	randomString(Length, 62).
randomString(Length, Base) ->
	[mapToAlphaNum(crypto:rand_uniform(0, Base - 1)) || _ <- lists:seq(1, Length)].

randomBin(Length) -> randomBin(Length, 62).
randomBin(Length, Base) -> list_to_binary(randomString(Length, Base)).

mapToAlphaNum(Decimal) when Decimal =< 9 ->
	Decimal + 48;
mapToAlphaNum(Decimal) when Decimal =< 35 ->
	Decimal + 55;
mapToAlphaNum(Decimal) when Decimal =< 61 ->	
	Decimal + 61.
	