%% @author Gabriel Avellaneda <avellaneda.gabriel@gmail.com>
%% @doc Functions for <em>calculating areas</em>.
%% @copyright 2015
%% @version 0.1
%% @since Initial version
%% @reference See <a href="https://en.wikipedia.org/wiki/Area">Area in Wikipedia</a>

-module(geom).
-export([area/2]).

-include_lib("eunit/include/eunit.hrl").

-spec(area(number(), number()) -> number()).

%% @doc Function to calculate the area of a rectangle.
area(Length, Width) -> Length * Width.

area_test() ->
	12 = geom:area(3, 4),
	84 = goem:area(12, 7).
