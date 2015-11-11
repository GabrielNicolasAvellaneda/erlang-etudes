-module(dijkstra).
-export([gcd/2]).

-include_lib("eunit/include/eunit.hrl").

-spec(gcd(number(), number()) -> number())
gcd(M, N) ->
	if
		M =:= N -> M;
		M > N -> gcd(M-N, N);
		true -> gcd(M, N-M)	
	end.	

gcd_test() ->
	?assert(gcd(12, 8) =:= 4),
	?assert(gcd(14, 21) =:= 7),
	?assert(gcd(125, 46) =:= 1),
	?assert(gcd(120, 36) =:= 12).
