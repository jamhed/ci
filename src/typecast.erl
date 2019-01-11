-module(typecast).
-export([l2b/1, b2l/1, b2a/1, b2ea/1, b2i/1, i2b/1, fmt/2, b2o/1, b2bool/1, b2int/1, b2float/1, b2list/1, b2b/1, keys_to_atoms/1]).

b2l(L) -> erlang:binary_to_list(L).

b2ea(B) when is_binary(B) -> erlang:binary_to_existing_atom(B, utf8).

b2a(B) when is_binary(B) -> erlang:binary_to_atom(B, utf8);
b2a(B) -> B.

i2b(I) when is_integer(I) -> erlang:integer_to_binary(I);
i2b(I) -> I.

fmt(F, A) -> lists:flatten(io_lib:format(F, A)).

b2o(null) -> undefined;
b2o(<<>>) -> undefined;
b2o(<<"undefined">>) -> undefined;
b2o(<<"true">>) -> true;
b2o(<<"false">>) -> false;
b2o(B) -> B.

b2bool(null) -> undefined;
b2bool(undefined) -> undefined;
b2bool(<<>>) -> undefined;
b2bool(<<"undefined">>) -> undefined;
b2bool(<<"true">>) -> true;
b2bool(<<"false">>) -> false;
b2bool(B) when is_boolean(B) -> B.

b2int(null) -> undefined;
b2int(undefined) -> undefined;
b2int(<<"undefined">>) -> undefined;
b2int(<<>>) -> undefined;
b2int(B) when is_integer(B)-> B;
b2int(B) when is_binary(B) -> erlang:binary_to_integer(B).

b2float(null) -> undefined;
b2float(undefined) -> undefined;
b2float(<<"undefined">>) -> undefined;
b2float(<<>>) -> undefined;
b2float(B) when is_integer(B)-> erlang:float(B);
b2float(B) when is_float(B)-> B;
b2float(B) when is_binary(B) ->
	try erlang:binary_to_integer(B) of
		V -> float(V)
	catch _:_ ->
		try erlang:binary_to_float(B) of
			V -> V
		catch _:_ ->
			erlang:error({float_error, B})
		end
	end.

b2list(null) -> [];
b2list(<<"undefined">>) -> [];
b2list(undefined) -> [];
b2list(<<>>) -> [];
b2list(L) when is_list(L) -> L.

b2i(null) -> undefined;
b2i(<<"undefined">>) -> undefined;
b2i(<<>>) -> undefined;
b2i(B) when is_binary(B) -> erlang:binary_to_integer(B);
b2i(B) -> B.

b2b(null) -> undefined;
b2b(<<>>) -> undefined;
b2b(<<"undefined">>) -> undefined;
b2b(B) -> B.

l2b(B) when is_binary(B) -> B;
l2b(B) when is_atom(B) -> erlang:atom_to_binary(B, utf8);
l2b(B) when is_list(B) -> erlang:list_to_binary(B).

keys_to_atoms(Map) -> maps:fold( fun(K,V,M) -> M#{ b2a(K) => V } end, #{}, Map).

