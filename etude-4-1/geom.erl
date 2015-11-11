%% @author Gabriel Avellaneda <avellaneda.gabriel@gmail.com>
%% @doc Functions for <em>calculating areas</em>.
%% @copyright 2015
%% @version 0.2
%% @since Initial version
%% @reference See <a href="https://en.wikipedia.org/wiki/Area">Area in Wikipedia</a>

-module(geom).
-export([area/3]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Function to calculate the area of a rectangle, triangle or ellipse
-spec(area(atom(), number(), number()) -> number()).

area(Shape, A, B) when A > 0, B > 0 ->
	case Shape of
		square -> A * A;
		rectangle -> A * B;
		triangle -> (A * B)/2.0;
		ellipse -> math:pi() * A * B
	end.

negative_dimensions_should_throw_exception_test() ->
	?assertError(function_clause, area(square, -1, -1)),
	?assertError(function_clause, area(rectangle, 0, 1)),
	?assertError(function_clause, area(triangle, -1, -2)),
	?assertError(function_clause, area(ellipse, -3, -2)).

square_area_test() ->
	4 = area(square, 2, 2).

rectangle_area_test() ->
	?assert(area(rectangle, 2, 4) =:= 8).

triangle_area_test() ->
	?assert(area(triangle, 3, 4) =:= 6.0).

ellipse_area_test() ->
	?assert(area(ellipse, 1, 1) =:= math:pi()).
