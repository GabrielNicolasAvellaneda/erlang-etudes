-module(geom).
-export([area/2]).

-include_lib("eunit/include/eunit.hrl").

-spec(area(number(), number()) -> number()).

area(Length, Width) -> Length * Width.

area_test() ->
	12 = geom:area(3, 4),
	84 = goem:area(12, 7).
