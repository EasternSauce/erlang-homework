-module(qsort).
-export([lessThan/2]).
-export([grtEqThan/2]).
-export([qs/1]).
-export([randomElems/3]).
-export([compareSpeeds/3]).

lessThan(List, Arg) ->
	[X || X <- List, X < Arg]. 


grtEqThan(List, Arg) ->
	[X || X <- List, X >= Arg].



qs([]) -> [];
qs([Pivot|Tail]) -> qs(lessThan(Tail,Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail,Pivot)).

randomElems(N, Min, Max) ->
	[random:uniform(Max - Min) + Min || _ <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
	{T1,_} = timer:tc(Fun1, [List]),
	{T2,_} = timer:tc(Fun2, [List]),
	T2 - T1.


