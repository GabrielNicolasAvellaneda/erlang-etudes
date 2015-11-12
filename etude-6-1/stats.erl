-module(stats).
-export([minimum/1]).

-include_lib("eunit/include/eunit.hrl").

-spec(minimum([number()]) -> number()).

minimum([A,B|Rest]) ->
		if
			A < B ->
				minimum([A|Rest]);
			true ->
				minimum([B|Rest])
		end;
minimum([H|[]])	->
		H.

minimum_test() ->
		?assert(minimum([2,5,1,3,8]) =:= 1),
		?assert(minimum([11]) =:= 11).
