-module(stats).
-export([minimum/1,minimum2/1,maximum/1,range/1,range2/1,min/2,max/2,mean/1,stdv/1,stdv2/1]).

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

-spec(range(list(number())) -> [number()]).
range(List) ->
		[minimum(List), maximum(List)].

-spec(range2(list(number())) -> [number()]).

-spec(max(number(), number()) -> number()).
max(A, B) ->
	if
		A > B  -> A;
		true -> B
	end.

-spec(min(number(), number()) -> number()).
min(A, B) ->
	if
		A < B -> A;
		true -> B
	end.

max_predicate(A, B) -> A > B.
min_predicate(A, B) -> not max_predicate(A, B).

%% @doc an abstraction that needs a name. It's a binary bifurcation. 
when_predicate(Predicate, True, False) ->
	case Predicate() of
		true -> True;
		false -> False
	end.

max2(A, B) ->
		when_predicate(fun () -> max_predicate(A, B) end, A, B).

min2(A, B) ->
		when_predicate(fun() -> min_predicate(A, B) end, A, B).

-spec(range3(list(number())) -> [number()]).
range3([H|Tail]) ->
		range3(Tail, H, H).
range3([H|Tail], Minimum, Maximum) ->
		range3(Tail, min2(H, Minimum), max2(H, Maximum));
range3([], Minimum, Maximum) ->
		[Minimum, Maximum].

%% @doc range2 returns the minimum and maximum value from a list of number as a list in a single pass.
range2([H|Tail]) ->
		range2(Tail, H, H).

range2([H|Tail], Minimum, Maximum) ->
		range2(Tail, stats:min(H,Minimum), stats:max(H, Maximum));
range2([], Minimum, Maximum) ->
		[Minimum, Maximum].
-spec sum([number()]) -> number().
sum(NumberList) ->
	lists:foldl(fun(A, B) -> A + B end, 0, NumberList). 

-spec mean([number()]) -> float().
mean(NumberList) ->
	Sum = fun(A,B) -> A + B end,
	SumOfNumbers = lists:foldl(Sum, 0, NumberList),
	SumOfNumbers/length(NumberList).

-spec stdv([number()]) -> float().
stdv(NumberList) ->
	Sum = sum(NumberList),
	SumOfSquares = sum([X*X || X <- NumberList]),
	N = length(NumberList),
	math:sqrt(((N * SumOfSquares) - (Sum * Sum)) / (N*(N-1))).

-spec stdv2_sum(number(), number()) -> number().
stdv2_sum(Value, Accumulator) ->
	[Sum, SumOfSquares] = Accumulator,
	[Sum+Value, SumOfSquares+ (Value * Value)].

-spec stdv2([number()]) -> float().
stdv2(NumberList) ->
	[Sum, SumOfSquares] = lists:foldl(fun stdv2_sum/2, [0, 0], NumberList),
	N = length(NumberList),
	math:sqrt(((N * SumOfSquares) - (Sum * Sum)) / (N * (N -1))).

minimum_test() ->
		?assert(minimum([2,5,1,3,8]) =:= 1),
		?assert(minimum([11]) =:= 11).

minimum2_test() ->
		?assert(minimum2([67,32,1,3,6]) =:= 1),
		?assert(minimum2([2]) =:= 2).

maximum_test() ->
		?assert(maximum([3,4,5,1,8,14,0]) =:= 14),
		?assert(maximum([2]) =:= 2).

range_test() ->
		?assert(range([2]) =:= [2,2]),
		?assert(range([4,6,1,7,-2,8,-17]) =:= [-17,8]).

range2_test() ->
		?assert(range2([5,6,7,8,9,-10,-50,-100]) =:= [-100,9]),
		?assert(range2([4]) =:= [4,4]).

range3_test() ->
		?assert(range3([5,28,3,1,7,4,3,-3]) =:= [-3, 28]),
		?assert(range3([5]) =:= [5,5]).

mean_test() ->
		?assert(mean([7,2,9]) =:= 6.0).

stdv_test() ->
		?assert(stdv([7,2,9]) =:= 3.605551275463989).

stdv2_test() ->
		?assert(stdv2([7,2,9]) =:= 3.605551275463989).

