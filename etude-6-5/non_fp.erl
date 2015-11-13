-module(non_fp).
-export([generate_teeth/2]).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-spec(generate_teeth(string(), number()) -> list()).
generate_teeth(Teeths, Probability) ->
		generate_teeth(Teeths, Probability, []).

-spec(generate_tooth(number()) -> list(number())).
generate_tooth(Probability) ->
		random:seed(os:timestamp()),
		R = random:uniform(),
		case R < Probability of
			true -> generate_tooth(2, 6, []);
			false -> generate_tooth(3, 6, [])
		end.  

-spec(generate_tooth(number(), number(), list()) -> list(number())).
generate_tooth(_BaseDepth, 0, Result) ->
		Result;
generate_tooth(BaseDepth, Left, Result) ->
		Random = random:uniform(3) - 2,
		Depth = BaseDepth + Random,
		generate_tooth(BaseDepth, Left -1, [Depth|Result]).

-spec(generate_teeth(string(), number(), list()) -> list()).
generate_teeth([], _Probability, Result) ->
		lists:reverse(Result);
generate_teeth([$F|Tail], Probability, Result) ->
		generate_teeth(Tail, Probability, [[0] | Result]);
generate_teeth([$T|Tail], Probability, Result) ->
		generate_teeth(Tail, Probability, [generate_tooth(Probability) | Result]).

generate_teeth_test() ->
		?assert(length(generate_teeth("FT", random:uniform())) =:= 2).

generate_tooth_test() ->
		?assert(length(generate_tooth(2, 6, [])) =:= 6).
