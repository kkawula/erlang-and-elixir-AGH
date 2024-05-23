%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2024 10:33 PM
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("Kamil").

-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, set_station/1, add_value/3, store_data/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).
-export([handle_event/4]).
-define(SERVER, ?MODULE).

%% State definitions
-define(WAITING_FOR_STATION, waiting_for_station).
-define(COLLECTING_DATA, collecting_data).

%% State variables
-record(state, {
  station = undefined,   % This will now store station identifier
  data = []
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_statem:stop(?MODULE).

set_station(StationIdentifier) ->
  gen_statem:call(?MODULE, {set_station, StationIdentifier}).

add_value(Time, Pollutant, Value) ->
  gen_statem:call(?MODULE, {add_value, Time, Pollutant, Value}).

store_data() ->
  gen_statem:call(?MODULE, store_data).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
  {ok, ?WAITING_FOR_STATION, #state{}}.

callback_mode() -> handle_event_function.

handle_event({call, From}, {set_station, StationIdentifier}, ?WAITING_FOR_STATION, State) ->
  {next_state, ?COLLECTING_DATA, State#state{station = StationIdentifier}, [{reply, From, ok}]};

handle_event({call, From}, {add_value, Time, Pollutant, Value}, ?COLLECTING_DATA, State = #state{data = Data}) ->
  {keep_state, State#state{data = [{Time, Pollutant, Value} | Data]}, [{reply, From, ok}]};

handle_event({call, From}, store_data, ?COLLECTING_DATA, State = #state{data = Data, station = Station}) ->
  Results = [pollution_gen_server:addValue(Station, Time, Pollutant, Value) || {Time, Pollutant, Value} <- Data],
  {next_state, ?WAITING_FOR_STATION, State#state{data = [], station = undefined}, [{reply, From, {stored, Results}}]};

handle_event(_Event, _From, _StateName, State) ->
  {keep_state, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% pollution_gen_server:addStation("pomidor", {1,1}).
%%pollution_value_collector_gen_statem:set_station("pomidor").
%%pollution_value_collector_gen_statem:add_value({7, 30, 1}, "PM10", 100).
%%pollution_value_collector_gen_statem:add_value({8, 30, 5}, "PM10", 200).

%%pollution_value_collector_gen_statem:store_data().

%%pollution_gen_server:getOneValue("pomidor", {7,30,1}, "PM10").
%%pollution_gen_server:getOneValue("pomidor", {8,30,5}, "PM10").