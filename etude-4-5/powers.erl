-module(powers).
-export([nth_root/2, raise/2]).

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

% @reference <a href="https://en.wikipedia.org/wiki/Newton%27s_method">Newton's method for nth root</a>
-spec(nth_root(number(), integer())-> number()).

%% @doc Calculates the Nth root of a number using the Newton-Raphson method.
nth_root(X, N) ->
	nth_root(X, N, X/2).

%% TODO: Look for the relation between nth root and zeros of a function. 
nth_root(X, N, A) ->
	F = raise(A, N) - X,
	Fprime = N * raise(A, N-1),
	Next = A - F / Fprime,
	Change = abs(Next - A),
	if
		Change < 1.0e-8 ->
			Next;
		true ->
			nth_root(X, N, Next)
	end.

nth_root_test() ->
	?assert((nth_root(4, 2) - 2.0) =< 1.0e-8),
	?assert((nth_root(27, 3) - 3.0) =< 1.0e-8).

raise_test() ->
	?assert(raise(5, 1) =:= 5),
	?assert(raise(2, 3) =:= 8),
	?assert(raise(1.2, 3) =:= 1.728),
	?assert(raise(2, 0) =:= 1),
	?assert(raise(2, -3) =:= 0.125).
