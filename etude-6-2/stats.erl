-module(stats).
-export([minimum/1,minimum2/1,maximum/1]).

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

-spec(maximum(list(number())) -> number()).

maximum([H|Tail]) ->
		maximum(Tail, H).

maximum([H|Tail], Result) ->
		case H > Result of
			true -> maximum(Tail, H);
			false -> maximum(Tail, Result)
		end;
maximum([], Result) -> Result.

minimum_test() ->
		?assert(minimum([2,5,1,3,8]) =:= 1),
		?assert(minimum([11]) =:= 11).

minimum2_test() ->
		?assert(minimum2([67,32,1,3,6]) =:= 1),
		?assert(minimum2([2]) =:= 2).

maximum_test() ->
		?assert(maximum([3,4,5,1,8,14,0]) =:= 14),
		?assert(maximum([2]) =:= 2).

