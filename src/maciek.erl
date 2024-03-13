%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Feb 2024 7:35 PM
%%%-------------------------------------------------------------------
-module(maciek).
-author("Kamil").

-export([power/2]).

power(_, 0) -> 1;
power(N, K) -> power(N, K-1) * N.

