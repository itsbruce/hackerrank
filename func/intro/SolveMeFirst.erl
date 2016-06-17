-module(solution).
-export([main/0]).

addTwo(A, B) ->
	A + B.

main() ->
	{ok, [Val1, Val2]} = io:fread('', "~d~d"),
	Output = addTwo(Val1, Val2),
	io:format("~p~n", [Output]).
