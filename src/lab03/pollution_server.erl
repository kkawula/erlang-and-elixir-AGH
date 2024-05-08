%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2024 1:33 AM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Kamil").

%% API
-export([start/0, init/0]).

start() ->
  spawn(?MODULE, init, []).

init() ->
  State = pollution:create_monitor(),
  loop(State).

loop(State) ->
  io:format("~p~n", [State]),
  receive
    {add_station, StationName, Location} ->
      NewState = pollution:add_station(StationName, Location, State),
      loop(NewState);
    {add_value, StationIdentifier, Time, Pollutant, Value} ->
      NewState = pollution:add_value(StationIdentifier, Time, Pollutant, Value, State),
      loop(NewState);
    {remove_value, StationIdentifier, Time, Pollutant} ->
      NewState = pollution:remove_value(StationIdentifier, Time, Pollutant, State),
      loop(NewState);
    {get_one_value, StationIdentifier, Time, Pollutant} ->
      Result = pollution:get_one_value(StationIdentifier, Time, Pollutant, State),
      io:format("~p~n", [Result]),
      loop(State);
    {get_station_mean, StationIdentifier, Pollutant} ->
      Result = pollution:get_station_mean(StationIdentifier, Pollutant, State),
      io:format("~p~n", [Result]),
      loop(State);
    {get_daily_mean, StationIdentifier, Date} ->
      Result = pollution:get_daily_mean(StationIdentifier, Date, State),
      io:format("~p~n", [Result]),
      loop(State);
    {does_point_fit_in_radius, StationIdentifier, Location, Radius} ->
      Result = pollution:does_point_fit_in_radius(StationIdentifier, Location, Radius, State),
      io:format("~p~n", [Result]),
      loop(State);
    {get_area_mean, Location, Radius, Start, End} ->
      Result = pollution:get_area_mean(Location, Radius, Start, End, State),
      io:format("~p~n", [Result]),
      loop(State);
    stop ->
      ok
  end.
