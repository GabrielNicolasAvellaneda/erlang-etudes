-module(dates).
-export([date_parts/1, julian/1]).

-include_lib("eunit/include/eunit.hrl").


-spec date_parts(string()) -> [number()].
date_parts(DateString) ->
	lists:map(fun (X) -> element(1, string:to_integer(X)) end, re:split(DateString, "-", [{return, list}])).

is_leap_year(Year) ->
		(Year rem 4 == 0 andalso Year rem 100 /= 0)
		orelse (Year rem 400 == 0).

-spec julian(string()) -> number().
julian(DateString) ->
		DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
		[Y,M,D] = date_parts(DateString),
		julian(Y, M, D, DaysPerMonth, 0).
-spec(julian(integer(), integer(), integer(), [integer()], integer()) -> integer()).
julian(Y, M, D, [H|Tail], Accumulator) ->
		case (12 - length(Tail) + 1) < M of
			true -> julian(Y, M, D, Tail, H + Accumulator);
			false -> julian(Y, M, D, [], H + Accumulator + D)
		end;
julian(Y, _M, _D, [], Accumulator) ->
		case M > 2 andalso is_leap_year(Y) of
			true -> Accumulator + 1;
			false -> Accumulator
		end.

date_parts_test() ->
	?assert(date_parts("2015-11-10") =:= [2015, 11, 10]).

julian_test() ->
	?assert(julian("2012-12-31") =:= 366),
	?assert(julian("2013-12-31") =:= 365),
	?assert(julian("2012-02-05") =:= 36).
