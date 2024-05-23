%%%-------------------------------------------------------------------
%% @doc borowka public API
%% @end
%%%-------------------------------------------------------------------

-module(borowka_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    borowka_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
