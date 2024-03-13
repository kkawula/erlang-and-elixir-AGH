%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Feb 2024 2:34 PM
%%%-------------------------------------------------------------------
-module(myLists).
-author("Kamil").

%% API
-export([contains/2, sumFloats/1, sumFloats/2, duplicateElements/1]).


contains(_, []) -> false;
contains(E, [E|_]) -> true;
contains(E, [_|T]) -> contains(E, T).

sumFloats(L) -> sumFloats(L, 0.0).
sumFloats([], Acc) -> Acc;
sumFloats([H|T], Acc) when is_float(H) -> sumFloats(T, H+Acc);
sumFloats([_|T], Acc) -> sumFloats(T, Acc).

duplicateElements([]) -> [];
duplicateElements([H|T]) -> [H,H|duplicateElements(T)].




