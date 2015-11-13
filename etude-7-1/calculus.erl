-module(calculus).
-export([derivative/2]).

-include_lib("eunit/include/eunit.hrl").

-spec(derivative(fun(), number()) -> number()).

derivative(Fun, X) ->
	Delta = 1.0e-10,
	(Fun(X + Delta) - Fun(X))/Delta.

derivative_test() ->
	?assert(derivative(fun (X) -> 2*X end, 0) =:= 2.0),
	?assert(derivative(fun math:sin/1, 0) =:= 1.0).
		