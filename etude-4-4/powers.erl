-module(powers).
-export([raise/2]).

-include_lib("eunit/include/eunit.hrl").

-spec(raise(number(), integer()) -> number()).

%% A Tail recursion version of power that uses an Accumulator

raise(X, N) when N >= 0 ->
	raise(X, N, 1);
raise(X, N) when N < 0 ->
	1 / raise(X, -N, 1).

raise(_X, 0, Accumulator) ->
	Accumulator;
raise(X, N, Accumulator) ->
	raise(X, N-1, X * Accumulator).

raise_test() ->
	?assert(raise(5, 1) =:= 5),
	?assert(raise(2, 3) =:= 8),
	?assert(raise(1.2, 3) =:= 1.728),
	?assert(raise(2, 0) =:= 1),
	?assert(raise(2, -3) =:= 0.125).
