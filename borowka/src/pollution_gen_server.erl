%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, terminate/2]).
-export([crash/0, addStation/2, addValue/4, removeValue/3, getAreaMean/3, getDailyMean/2, getOneValue/3, getStationMean/2]).

-define(SERVER, ?MODULE).

-record(pollution_gen_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, pollution:create_monitor()}.

handle_call({add_station, StationName, Location}, _From, State) ->
  NewState = pollution:add_station(StationName, Location, State),
  case NewState of
    {error, Reason} -> {reply, {error, Reason}, State};
    _ -> {reply, ok, NewState}
  end;

handle_call({add_value, StationIdentifier, Time, Pollutant, Value}, _From, State) ->
  NewState = pollution:add_value(StationIdentifier, Time, Pollutant, Value, State),
  case NewState of
    {error, Reason} -> {reply, {error, Reason}, State};
    _ -> {reply, ok, NewState}
  end;

handle_call({remove_value, StationIdentifier, Time, Pollutant}, _From, State) ->
  NewState = pollution:remove_value(StationIdentifier, Time, Pollutant, State),
  case NewState of
    {error, Reason} -> {reply, {error, Reason}, State};
    _ -> {reply, ok, NewState}
  end;

handle_call({get_one_value, StationIdentifier, Time, Pollutant}, _From, State) ->
  {reply, pollution:get_one_value(StationIdentifier, Time, Pollutant, State), State};

handle_call({get_station_mean, StationIdentifier, Pollutant}, _From, State) ->
  {reply, pollution:get_station_mean(StationIdentifier, Pollutant, State), State};

handle_call({get_daily_mean, Pollutant, Time}, _From, State) ->
  {reply, pollution:get_daily_mean(Pollutant, Time, State), State};

handle_call({get_area_mean, Pollutant, Location, Radius}, _From, State) ->
  {reply, pollution:get_area_mean(Pollutant, Location, Radius, State), State};

handle_call({crash}, _From, State) ->
  {reply, pollution:crash(), State}.

terminate(_Reason, _State = #pollution_gen_server_state{}) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
crash() ->
  gen_server:call(?SERVER, {crash}).

addStation(StationName, Location) ->
  gen_server:call(?SERVER, {add_station, StationName, Location}).

addValue(StationIdentifier, Time, Pollutant, Value) ->
  gen_server:call(?SERVER, {add_value, StationIdentifier, Time, Pollutant, Value}).

removeValue(StationIdentifier, Time, Pollutant) ->
  gen_server:call(?SERVER, {remove_value, StationIdentifier, Time, Pollutant}).

getOneValue(StationIdentifier, Time, Pollutant) ->
  gen_server:call(?SERVER, {get_one_value, StationIdentifier, Time, Pollutant}).

getStationMean(StationIdentifier, Pollutant) ->
  gen_server:call(?SERVER, {get_station_mean, StationIdentifier, Pollutant}).

getDailyMean(Pollutant, Time) ->
  gen_server:call(?SERVER, {get_daily_mean, Pollutant, Time}).

getAreaMean(Pollutant, Location, Radius) ->
  gen_server:call(?SERVER, {get_area_mean, Pollutant, Location, Radius}).

%%%

% Example usage:
%%pollution_gen_server:crash().

%%pollution_gen_server:addStation("pomidor", {1,1}).
%%
%%pollution_gen_server:addValue({1,1}, {10,10,10}, "PM2", 13.3).
%%pollution_gen_server:addValue("pomidor", {11,10,10}, "PM2", 70.3).
%%
%%pollution_gen_server:getOneValue("pomidor", {10,10,10}, "PM2").