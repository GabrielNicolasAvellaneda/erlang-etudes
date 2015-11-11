-module(powers).
-export([raise/2, raise_tail/2]).

-include_lib("eunit/include/eunit.hrl").

-spec(raise(number(), integer()) -> number()).
-spec(raise_tail(number(), integer()) -> number()).
-spec(raise_tail(number(), integer(), number()) -> number()).

raise(_X, 0) -> 1;
raise(X, 1) -> X;
raise(X, N) when N > 1 -> X * raise(X, N-1);
raise(X, N) when N < 0 -> 1.0 / raise(X, -N).

raise_tail(X, N) when N >= 0 ->
	raise_tail(X, N, 1);
raise_tail(X, N) when N < 0 ->
	1 / raise_tail(X, -N, 1).

raise_tail(_X, 0, Acc) ->
	Acc;
raise_tail(X, N, Acc) ->
	raise_tail(X, N-1, Acc*X).

raise_test() ->
	?assert(raise(5, 1) =:= 5),
	?assert(raise(2, 3) =:= 8),
	?assert(raise(1.2, 3) =:= 1.728),
	?assert(raise(2, 0) =:= 1),
	?assert(raise(2, -3) =:= 0.125).

raise_tail_test() ->
	?assert(raise_tail(5, 1) =:= 5),
	?assert(raise_tail(2, 3) =:= 8),
	?assert(raise_tail(1.2, 3) =:= 1.728),
	?assert(raise_tail(2, 0) =:= 1),
	?assert(raise_tail(2, -3) =:= 0.125).

