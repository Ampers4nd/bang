-module(crypto_helper).

-export([paddedString/2, unpaddedString/2, aes_cbc_encrypt/3, aes_cbc_decrypt/3]).	
-export([start_crypto_apps/0, extract_rsa_key_from_file/1, extract_rsa_key/1, extract_private_key_from_file/2, extract_private_key/2]).

paddedString(Unpadded, BlockSize) when is_list(Unpadded), is_integer(BlockSize), BlockSize =< 256 ->
	NumOctets = BlockSize div 8,
	Mod = length(Unpadded) rem NumOctets,
	Pad = lists:duplicate(NumOctets - Mod, Mod - 1),
	Unpadded ++ Pad. 

unpaddedString(Padded, BlockSize) when is_list(Padded), is_integer(BlockSize), BlockSize =< 16 ->
	NumOctets = BlockSize div 8,
	PadChar = lists:last(Padded),
	lists:sublist(Padded, length(Padded) - (NumOctets - PadChar - 1)).

aes_cbc_encrypt(Key, IVec, UnpaddedPlainText) ->
	BlockSize = length(Key) * 8,
	Padded = paddedString(UnpaddedPlainText, BlockSize),
	case BlockSize of 
		128 -> 
			binary_to_list(crypto:aes_cbc_128_encrypt(list_to_binary(Key), list_to_binary(IVec), Padded));
		256 ->
			binary_to_list(crypto:aes_cbc_256_encrypt(list_to_binary(Key), list_to_binary(IVec), Padded))
	end.


aes_cbc_decrypt(Key, IVec, CipherText) ->
	BlockSize = length(Key),
	PaddedBin = case {BlockSize, length(IVec)} of 
		{128, 128} -> 
			crypto:aes_cbc_128_decrypt(list_to_binary(Key), list_to_binary(IVec), CipherText);
		{256, 256} ->
			crypto:aes_cbc_256_decrypt(list_to_binary(Key), list_to_binary(IVec), CipherText)
	end,
	unpaddedString(binary_to_list(PaddedBin), BlockSize).

start_crypto_apps() ->
	[application:start(Mod) || Mod <- [crypto, public_key, ssl]].


%extract public or private key w/o password
extract_rsa_key_from_file(Filename) ->
	{ok, PEMBin} = file:read_file(Filename),
	extract_rsa_key(PEMBin).

extract_rsa_key(PEMBin) when is_binary(PEMBin) ->
	[Entry] = public_key:pem_decode(PEMBin),
	public_key:pem_entry_decode(Entry).

%extract private key w/ password
extract_private_key_from_file(filename, Password)->
	{ok, PrivatePEMBin} = file:read_file(filename),
	extract_private_key(PrivatePEMBin, Password).

extract_private_key(PrivatePEMBin, Password) ->
	[PrivateEntry] = public_key:pem_decode(PrivatePEMBin),
	public_key:pem_entry_decode(PrivateEntry, Password).
