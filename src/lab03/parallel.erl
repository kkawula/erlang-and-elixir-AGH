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
-export([generateList/1, getDistance/2, find_for_person/2, find_for_person/3, run/0, find_closest/2,find_closest_parrallel/2]).


generateList(N)->
  [{rand:uniform(10000),rand:uniform(10000)} || _<-lists:seq(1,N)].

getDistance({X1, Y1}, {X2, Y2}) ->
  math:sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2)*(Y1 - Y2)).

find_for_person(PersonLocation, SensorsLocations) ->
  Dists = [{getDistance(PersonLocation, SensorLocation), SensorLocation} || SensorLocation <- SensorsLocations],
  lists:min(Dists).

find_closest(PeopleLocations, SensorsLocations) ->
  [find_for_person(PersonLocation, SensorsLocations) || PersonLocation <- PeopleLocations].

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  ParentPID ! find_for_person(PersonLocation, SensorsLocations).

%%find_closest_parrallel(PeopleLocations, SensorsLocations) ->
%%  [spawn(fun() -> find_for_person(PersonLocation, SensorsLocations, self()) end) || PersonLocation <- PeopleLocations],
%%  [receive Result -> Result end || _ <-PeopleLocations].

find_closest_parrallel(PeopleLocations, SensorsLocations) ->
  [spawn(?MODULE, find_for_person, [PersonLocation, SensorsLocations, self()]) || PersonLocation <- PeopleLocations],
  lists:min([receive Result -> Result end || _ <-PeopleLocations]).


run() ->
  SensorList = [{1, 10}, {-1, -10}],
  PeopleList = [{0, 0}, {12, 12}],
  find_closest_parrallel(PeopleList, SensorList).


