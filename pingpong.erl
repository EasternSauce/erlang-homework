-module(pingpong).
-export([start/0]).
-export([stop/0]).
-export([play/1]).
-export([loop/0]).


start() ->
	register(ping, spawn(?MODULE, loop, [])),
	register(pong, spawn(?MODULE, loop, [])).

stop() ->
	ping ! stop,
	pong ! stop.

play(N) ->
	ping ! {pong, N}.

loop() ->
	receive
		{_, 0} ->
			io:format("Process ends with N=0"),
			stop(),
			pingpong:loop();
		{Pid, N} ->
			timer:sleep(50),
			io:format("Server got ~w, returning ~w ~n", [N, N-1]),
			Pid ! {self(), N-1},
			pingpong:loop();
		stop -> stopped
	end.

