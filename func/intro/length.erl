% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

-module(solution).
-export([main/0]).

main() ->
	io:format("~w~n",[len(lines())]).

lines() ->
    case io:get_line("") of
        eof -> [];
        Line -> lines([Line])
    end.

lines(XS) ->
    case io:get_line("") of
        eof -> XS;
        Line -> lines([Line|XS])
    end.

len([]) -> 0;
len([_|Rest]) ->
    len(1,Rest).

len(N, []) -> N;
len(N, [_|Rest]) ->
    len(N + 1, Rest).
