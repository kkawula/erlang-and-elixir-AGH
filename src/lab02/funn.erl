%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2024 2:31 PM
%%%-------------------------------------------------------------------
-module(funn).
-author("Kamil").

%% API
-export([zamiana_liter/1, wypluj_liste/1, wypluj_liste_foldl/1, wypluj_liste_lists/1]).

%%pomidor = fun(String) -> lists:map(fun ($o) -> $a; ($e) -> $o; (Other) -> Other end ,String) end.

zamiana_liter(String) ->
  Fun = fun($o) -> $a; ($e) -> $o; (Other) -> Other end,
  lists:map(Fun, String).


wypluj_liste(Lista) -> lists:filter(fun(X) -> X rem 3 =:= 0 end, Lista).

wypluj_liste_foldl(Lista) -> lists:foldl(fun(X, Acc) -> if X rem 3 =:= 0 -> [X | Acc]; true -> Acc end end, [], Lista).

wypluj_liste_lists(Lista) -> [X || X <- Lista, X rem 3 =:= 0].