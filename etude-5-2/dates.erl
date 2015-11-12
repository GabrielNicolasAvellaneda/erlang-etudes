-module(dates).
-export([date_parts/1]).

-include_lib("eunit/include/eunit.hrl").


-spec date_parts(string()) -> [number()].
date_parts(DateString) ->
	lists:map(fun (X) -> {N, _} = string:to_integer(binary:bin_to_list(X)), N end, re:split(DateString, "-")).

date_parts_test() ->
		?assert(date_parts("2015-11-10") =:= [2015, 11, 10]).
