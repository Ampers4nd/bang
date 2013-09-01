-module(bang_crypto).

-export([hash/1, sha1/1, sha256/1, sha512/1, randomString/1]).

hash(Data) ->
	sha512(Data).

sha1(Data) ->
	hexstring(sha1, crypto:hash(sha, Data)).

sha256(Data) ->
	hexstring(sha256, crypto:hash(sha256, Data)). 

sha512(Data) ->
	hexstring(sha512, crypto:hash(sha512, Data)). 

hexstring(sha1, Binary) ->
	hexstring160(Binary);
hexstring(sha256, Binary) ->
	hexstring256(Binary);
hexstring(sha512, Binary) ->
	hexstring512(Binary).		


%%i took this stuff from here: http://www.enchantedage.com/hex-format-hash-for-md5-sha1-sha256-and-sha512
hexstring160(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X])).

hexstring256(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X])).	

hexstring512(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).	        

randomString(Length) ->
	list_to_binary([mapToAlphaNum(crypto:rand_uniform(0, 61)) || _ <- lists:seq(1, Length)]).

mapToAlphaNum(Decimal) when Decimal =< 9 ->
	Decimal + 48;
mapToAlphaNum(Decimal) when Decimal =< 35 ->
	Decimal + 55;
mapToAlphaNum(Decimal) when Decimal =< 61 ->	
	Decimal + 60.