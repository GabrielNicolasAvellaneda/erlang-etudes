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

area(square, A, A) when A > 0 -> A * A;
area(rectangle, A, B) when A > 0, B > 0 -> A * B;
area(triangle, A, B) when A > 0, B > 0 -> A * B / 2.0;
area(ellipse, A, B) when A > 0, B > 0 -> math:pi() * A * B.

negative_dimensions_should_throw_exception_test() ->
	try
		area(square, -1, -1),
		throw(should_throw_match_exception)
	catch
		error:function_clause -> ok
	end.

square_area_test() ->
	4 = area(square, 2, 2).

square_area_bad_params_test() ->
	try
		area(square, 3, 4),
		throw(should_throw_function_clause_exception)
	catch
		error:function_clause -> ok
	end.

rectangle_area_test() ->
	?assert(area(rectangle, 2, 4) =:= 8).

triangle_area_test() ->
	?assert(area(triangle, 3, 4) =:= 6.0).

ellipse_area_test() ->
	?assert(area(ellipse, 1, 1) =:= math:pi()).
