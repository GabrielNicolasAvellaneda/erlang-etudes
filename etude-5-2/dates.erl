-module(dates).
-export([date_parts/1]).

-include_lib("eunit/include/eunit.hrl").


-spec date_parts(string()) -> [number()].
date_parts(DateString) ->
	lists:map(fun (X) -> element(1, string:to_integer(X)) end, re:split(DateString, "-", [{return, list}])).

date_parts_test() ->
		?assert(date_parts("2015-11-10") =:= [2015, 11, 10]).
