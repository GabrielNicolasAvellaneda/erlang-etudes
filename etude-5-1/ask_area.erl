-module(ask_area).
-export([area/0]).

-spec(char_to_shape(char())->atom()).
char_to_shape(Char) ->
	case Char of
		$R -> rectangle;
		$r -> rectangle;
		$T -> triangle;
		$t -> triangle;
		$E -> ellipse;
		$e -> ellipse;
		_ -> unknown
	end.

-spec(get_number(string()) -> number()).
get_number(NumberString) ->
	{Result, _} = string:to_float(NumberString),
	case Result of	
		error -> 
			{N, _} = string:to_integer(NumberString),
			N;
		_ -> Result
	end.

%% get_dimensions returns a number if it can parse a number. Or an atom if there is an error
-spec(get_dimensions(string(), string()) -> {number(), number()}).
get_dimensions(PromptA, PromptB) ->
	A = io:get_line("Enter " ++  PromptA ++ " > "),
	B = io:get_line("Enter " ++ PromptB ++ " > "),
	{get_number(A), get_number(B)}.

calculate(unknown, _, Error) -> io:format("~s~n", [Error]);
calculate(_, error, _) -> io:format("Error in first number.~n");
calculate(_, _, error) -> io:format("Error in second number.~n");
calculate(_, A, B) when A =< 0; B =< 0 -> io:format("Both number must be greater than zero.");
calculate(Shape, A, B) -> geom:area(Shape, A, B).

area() ->
	ShapeString = io:get_line("R)ectangle, T)riangle, or E)llipse > "),
	Shape = char_to_shape(hd(ShapeString)),
	case Shape of
		rectangle -> Numbers = get_dimensions("width", "height");
		triangle -> Numbers = get_dimensions("base", "height");
		ellipse -> Numbers = get_dimensions("major axis", "minor axis");
		unknown -> Numbers = {error, "Unknown shape " ++ ShapeString} 
	end,
	{A, B} = Numbers,
	calculate(Shape, A, B).
