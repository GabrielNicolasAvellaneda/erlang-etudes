-module(teeth).
-export([alert/1]).

-include_lib("eunit/include/eunit.hrl").

-spec(alert(list()) -> list(number())).
alert(PocketDepths) ->
	alert(PocketDepths, 1, []).

-spec(requires_attention(list()) -> boolean()).
requires_attention([]) ->
		false;
requires_attention([H|_]) when H >= 4 -> true;
requires_attention([_|Tail]) -> requires_attention(Tail).

-spec(alert(list(), number(),list()) -> list(number)).
alert([], _Index, Result) ->
	lists:reverse(Result);
alert([H|Tail], Index, Result) ->
	case requires_attention(H) of
		true -> alert(Tail, Index + 1, [Index | Result]);
		false -> alert(Tail, Index + 1, Result)
	end.

alert_test() ->
	PocketDepths = [[0], [2,2,1,2,2,1], [3,1,2,3,2,3],
	[3,1,3,2,1,2], [3,2,3,2,2,1], [2,3,1,2,1,1],
	[3,1,3,2,3,2], [3,3,2,1,3,1], [4,3,3,2,3,3],
	[3,1,1,3,2,2], [4,3,4,3,2,3], [2,3,1,3,2,2],
	[1,2,1,1,3,2], [1,2,2,3,2,3], [1,3,2,1,3,3], [0],
	[3,2,3,1,1,2], [2,2,1,1,3,2], [2,1,1,1,1,2],
	[3,3,2,1,1,3], [3,1,3,2,3,2], [3,3,1,2,3,3],
	[1,2,2,3,3,3], [2,2,3,2,3,3], [2,2,2,4,3,4],
	[3,4,3,3,3,4], [1,1,2,3,1,2], [2,2,3,2,1,3],
	[3,4,2,4,4,3], [3,3,2,1,2,3], [2,2,2,2,3,3],
	[3,2,3,2,3,2]],

	?assert(alert(PocketDepths) =:= [9,11,25,26,29]).
