%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2024 1:48 PM
%%%-------------------------------------------------------------------
-module(parallel).
-author("Kamil").

%% API
-export([generateList/1, calculateDistance/2, find_for_person/2, run/0, find_closest/2,find_closest_parrallel/2]).


generateList(N) ->
  [{rand:uniform(360) - 180, rand:uniform(180) - 90} || _ <- lists:seq(1, N)].

calculateDistance({sx, sy}, {px, py}) ->
  math:sqrt(math:pow(sx - px, 2) + math:pow(sy - py, 2)).

find_for_person(PersonLocation, SensorsLocations) ->
  Dists = [{calculateDistance(SensorLocation, PersonLocation), SensorLocation} || SensorLocation <- SensorsLocations],
  lists:min(Dists).

find_closest(PeopleLocations, SensorsLocations) ->
  [find_for_person(PersonLocation, SensorsLocations) || PersonLocation <- PeopleLocations].

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  ParentPID ! find_for_person(PersonLocation, SensorsLocations).

find_closest_parrallel(PeopleLocations, SensorsLocations) ->
  [spawn(fun() -> find_for_person(PersonLocation, SensorsLocations, self()) end) || PersonLocation <- PeopleLocations],
  [receive {_, Result} -> Result end || _ <- PeopleLocations].

%%find_closest_parrallel(PeopleLocations, SensorsLocations) ->
%%  [spawn(?MODULE, find_for_person, [PersonLocation, SensorsLocations, self()]) || PersonLocation <- PeopleLocations],
%%  Dist=[receive Result -> Result end || _ <-PeopleLocations],
%%  lists:min(Dist).


run() ->
  SensorList = generateList(1000),
  PeopleList = generateList(20000),
  find_for_person(PeopleList, SensorList).


