%% @author Gabriel Avellaneda <avellaneda.gabriel@gmail.com>
%% @doc Functions for <em>calculating areas</em>.
%% @copyright 2015
%% @version 0.2
%% @since Initial version
%% @reference See <a href="https://en.wikipedia.org/wiki/Area">Area in Wikipedia</a>

-module(geom).
-export([area/1, area/3]).

-include_lib("eunit/include/eunit.hrl").

-spec(area(atom(), number(), number()) -> number()).
-spec(area({atom(), number(), number()}) -> number()).

%% @doc Function to calculate the area of a rectangle, triangle or ellipse
area({Shape, A, B}) ->
	area(Shape, A, B).

%% @doc Function to calculate the area of a rectangle, triangle or ellipse
area(square, A, A) when A > 0 -> A * A;
area(rectangle, A, B) when A > 0, B > 0 -> A * B;
area(triangle, A, B) when A > 0, B > 0 -> A * B / 2.0;
area(ellipse, A, B) when A > 0, B > 0 -> math:pi() * A * B;
%% Catch all function clause
area(_Shape, _A, _B) -> %% catch all 
	0.

area_with_wrong_tuple_test() ->
	?assertError(function_clause, area({rectangle, 3})).

area_with_tuple_test() ->
	12 = area({rectangle, 3, 4}).

square_area_test() ->
	4 = area(square, 2, 2).

rectangle_area_test() ->
	?assert(area(rectangle, 2, 4) =:= 8).

square_area_non_equal_params_test() ->
	0 = area('square', -1, 2).

rectangle_area_negative_params_test() ->
	0 = area('rectangle', -1, 2).

triangle_area_test() ->
	?assert(area(triangle, 3, 4) =:= 6.0).

ellipse_area_test() ->
	?assert(area(ellipse, 1, 1) =:= math:pi()).
