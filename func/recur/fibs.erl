% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

-module(solution).
-export([main/0]).

main() ->
	{ok,[X]} = io:fread("","~d"),
    io:format("~w~n",[fib(X)]).

fib(1) ->
    0;
fib(2) ->
    1;
fib(N) ->
    fib(N - 3, 0, 1).

fib(0, X, Y) ->
    X + Y;
fib(N, X, Y) ->
    fib(N - 1, Y, (X + Y)).
