%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2024 10:37 PM
%%%-------------------------------------------------------------------
-module(rozgrzewka02).
-author("Kamil").

%% API
-export([testRecord/0]).

-record(grupa, {nazwa, licznosc, stan=aktywna}).

testRecord() ->
  G1 = #grupa{nazwa="A", licznosc=10},
  G2 = #grupa{nazwa="B", licznosc=20, stan=nieaktywna},
  [G1, G2].


