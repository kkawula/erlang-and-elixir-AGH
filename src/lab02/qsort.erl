%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2024 1:33 PM
%%%-------------------------------------------------------------------
-module(qsort).
-author("Kamil").

%% API
-export([less_than/2, grt_eq_than/2, qs/1, random_elems/3, sorted_elems/1, compare_speeds/3]).

less_than(List, Arg) -> [X || X <- List, X < Arg].
grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([A]) -> [A];
qs([Pivot|Tail]) -> qs( less_than(Tail, Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail, Pivot) ).

random_elems(N, Min, Max) -> [rand:uniform(Max-Min+1)+Min || _ <- lists:seq(1,N)].
sorted_elems(N) -> lists:seq(1,N).

compare_speeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Time for Fun1: ~p~nTime for Fun2: ~p~n", [Time1, Time2]).


