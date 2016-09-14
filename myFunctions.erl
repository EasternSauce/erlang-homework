-module(myFunctions).
-export([power/2]).
-export([divisibleBy/2]).
-export([contains/2]).
-export([duplicateElements/1]).
-export([toBinary/1]).

power(A, N) ->
	case N of
		 0 -> 1;
		 _ -> A * power(A, N-1)
	end.

contains([], _) ->
	false;

contains([H | T], A) ->
	if
		H == A -> true;
		true -> contains(T, A)
	end.

duplicateElements([]) ->
	[];

duplicateElements([H|T]) ->
	[H,H] ++ duplicateElements(T).

divisibleBy([], _) ->
	[];

divisibleBy([H | T], Dzielnik) ->
	if
		H rem Dzielnik == 0 -> [H] ++ divisibleBy(T, Dzielnik);
		true -> divisibleBy(T, Dzielnik)
	end.

toBin(0) ->
	[];

toBin(N) when trunc(N) rem 2 == 0 ->
	toBin(trunc(N/2)) ++ [0];

toBin(N) when trunc(N) rem 2 == 1 ->
	toBin(trunc(N/2)) ++ [1].

toBinary(N) ->
	M = toBin(N),
	case M of
		[] -> [0];
		_ -> M
	end.
