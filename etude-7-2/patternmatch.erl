-module(patternmatch).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


names_of_males_over_40(People) ->
	[Name || {Name, Gender, Age} <- People, Gender =:= $M, Age >= 40].

names_of_males_or_over_40(People) ->
	[Name || {Name, Gender, Age} <- People, (Gender =:= $M) orelse (Age > 40)].

names_of_males_over_40_test() ->
	People = [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
	  {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}],
	?assert(names_of_males_over_40(People) =:= ["Tran", "Elias"]).

names_of_males_or_over_40_test() ->
	People = [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
		  {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}],
	?assert(names_of_males_or_over_40(People) =:= ["Federico", "Kim", "Tran", "Elias"]).


