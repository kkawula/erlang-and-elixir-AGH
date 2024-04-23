%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2024 1:30 PM
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Kamil").

%% API
-export([start/0, ping_loop/0, pong_loop/0]).

start() ->
  register(ping, spawn(fun() -> ping_loop() end)),
  register(pong, spawn(pingpong, pong_loop, [])),
  ping ! 100.



ping_loop() ->
  receive
    0 -> io:format("Ping received 0~n"),
      timer:sleep(100);
    N -> io:format("Ping received ~p~n", [N]),
      timer:sleep(100),
      pong ! N-1,
      ping_loop()
  end.

pong_loop() ->
  receive
    0 -> io:format("Pong received 0~n"),
      timer:sleep(100);
    N -> io:format("Pong received ~p~n", [N]),
      timer:sleep(100),
      ping ! N-1,
      pong_loop()
  end.
