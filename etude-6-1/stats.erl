-module(stats).
-export([minimum/1,minimum2/1]).

-include_lib("eunit/include/eunit.hrl").

-spec(minimum([number()]) -> number()).
-spec(minimum2(list(number())) -> number()).

minimum2(NumberList) ->
		minimum2(NumberList, hd(NumberList)).

minimum2([], Result) ->
		Result;
minimum2([Head|Tail], Result) ->
		case Head < Result of
			true -> minimum2(Tail, Head);
			false -> minimum2(Tail, Result)
		end.

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

minimum2_test() ->
		?assert(minimum2([67,32,1,3,6]) =:= 1),
		?assert(minimum2([2]) =:= 2).
		