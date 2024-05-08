%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2024 2:58 PM
%%%-------------------------------------------------------------------
-module(pollution).
-author("Kamil").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3, does_point_fit_in_radius/3, get_area_mean/4]).


create_monitor() -> #{}.

add_station(StationName, Location, Monitor) ->
  StationKey = {StationName, Location},
  StationWithKey = maps:find(StationKey, Monitor),

  case StationWithKey of
    {ok, _} ->
      {error, "Station with this name and location already exists"};
    error ->
      SameLocation = maps:filter(fun({_, Loc}, _) -> Loc == Location end, Monitor),
      SameName = maps:filter(fun({Name, _}, _) -> Name == StationName end, Monitor),

      case {maps:size(SameLocation), maps:size(SameName)} of
        {0, 0} ->
          NewMonitor = Monitor#{StationKey => []},
          NewMonitor;
        _ ->
          {error, "Station with this location or name already exists"}
      end
  end.

add_value(StationIdentifier, Time, Pollutant, Value, Monitor) ->
  FindStation = fun(Key, _, Acc) ->
    case Key of
      {Name, _} when Name == StationIdentifier -> [Key | Acc];
      {_, Loc} when Loc == StationIdentifier -> [Key | Acc];
      _ -> Acc
    end
  end,
  MatchingStations = maps:fold(FindStation, [], Monitor),

  case MatchingStations of
    [StationKey] ->
      case maps:find(StationKey, Monitor) of
        {ok, Readings} ->
          DuplicateReading = lists:any(fun({ExistingTime, ExistingPollutant, _}) ->
            ExistingTime == Time andalso ExistingPollutant == Pollutant
                                       end, Readings),
          case DuplicateReading of
            false ->
              NewReadings = [{Time, Pollutant, Value} | Readings],
              NewMonitor = Monitor#{StationKey => NewReadings},
              NewMonitor;
            true ->
              {error, "Reading with this time and pollutant already exists"}
          end;
        error ->
          {error, "Station does not exist"}
      end;
    _ ->
      {error, "Station does not exist or multiple matches found"}
  end.

remove_value(StationIdentifier, Time, Pollutant, Monitor) ->
  FindStation = fun(Key, _, Acc) ->
    case Key of
      {Name, _} when Name == StationIdentifier -> [Key | Acc];
      {_, Loc} when Loc == StationIdentifier -> [Key | Acc];
      _ -> Acc
    end
  end,
  MatchingStations = maps:fold(FindStation, [], Monitor),

  case MatchingStations of
    [StationKey] ->
      case maps:find(StationKey, Monitor) of
        {ok, Readings} ->
          DoExist = lists:any(fun({ExistingTime, ExistingPollutant, _}) ->
            ExistingTime == Time andalso ExistingPollutant == Pollutant
                                       end, Readings),
          case DoExist of
            true ->
              NewReadings = lists:filter(fun({ExistingTime, ExistingPollutant, _}) ->
                ExistingTime /= Time orelse ExistingPollutant /= Pollutant
                                          end, Readings),
              NewMonitor = Monitor#{StationKey => NewReadings},
              NewMonitor;
            false ->
              {error, "Reading does not exist"}
          end;
        error ->
          {error, "Station does not exist"}
      end;
    _ ->
      {error, "Station does not exist or multiple matches found"}
  end.

get_one_value(StationIdentifier, Time, Pollutant, Monitor) ->
  FindStation = fun(Key, _, Acc) ->
    case Key of
      {Name, _} when Name == StationIdentifier -> [Key | Acc];
      {_, Loc} when Loc == StationIdentifier -> [Key | Acc];
      _ -> Acc
    end
  end,
  MatchingStations = maps:fold(FindStation, [], Monitor),

  case MatchingStations of
    [StationKey] ->
      case maps:find(StationKey, Monitor) of
        {ok, Readings} ->
          FilteredReadings = lists:filter(fun({ExistingTime, ExistingPollutant, _}) ->
            ExistingTime == Time andalso ExistingPollutant == Pollutant
                                          end, Readings),
          case FilteredReadings of
            [] ->
              {error, "Reading does not exist"};
            [{_, _, Value} | _] ->
              Value
          end;
        error ->
          {error, "Station does not exist"}
      end;
    _ ->
      {error, "Station does not exist or multiple matches found"}
  end.

get_station_mean(StationIdentifier, Pollutant, Monitor) ->
  FindStation = fun(Key, _, Acc) ->
    case Key of
      {Name, _} when Name == StationIdentifier -> [Key | Acc];
      {_, Loc} when Loc == StationIdentifier -> [Key | Acc];
      _ -> Acc
    end
  end,
  MatchingStations = maps:fold(FindStation, [], Monitor),

  case MatchingStations of
    [StationKey] ->
      case maps:find(StationKey, Monitor) of
        {ok, Readings} ->
          FilteredReadings = lists:filter(fun({_, ExistingPollutant, _}) ->
            ExistingPollutant == Pollutant
                                          end, Readings),
          Values = lists:map(fun({_, _, Value}) -> Value end, FilteredReadings),
          case Values of
            [] ->
              {error, "No readings for this pollutant"};
            _ ->
              lists:sum(Values) / length(Values)
          end;
        error ->
          {error, "Station does not exist"}
      end;
    _ ->
      io:format("Matching stations: ~p~n", [MatchingStations]),
      {error, "Station does not exist or multiple matches found"}
  end.

get_daily_mean(Pollutant, Date, Monitor) ->

  Readings = maps:fold(fun(_, Value, Acc) -> Value ++ Acc end, [], Monitor),
  FilteredReadings = lists:filter(fun({{ExistingDate, _}, ExistingPollutant, _}) ->
    ExistingDate == Date andalso ExistingPollutant == Pollutant
                                  end, Readings),

  case FilteredReadings of
    [] ->
      {error, "No readings"};
    _ ->
      Values = lists:map(fun({_, _, Value}) -> Value end, FilteredReadings),
      case Values of
        [] ->
          {error, "No readings for this pollutant"};
        _ ->
          lists:sum(Values) / length(Values)
      end
  end.

does_point_fit_in_radius({X1, Y1}, {X2, Y2}, Radius) ->
  Distance = math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)),
  Distance =< Radius.

get_area_mean(Pollutant, Location, Radius, Monitor) ->
  FindStation = fun(Key, _, Acc) ->
    case Key of
      {_, Loc} ->
        case does_point_fit_in_radius(Loc, Location, Radius) of
          true -> [Key | Acc];
          false -> Acc
        end;
      _ -> Acc
    end
  end,

  MatchingStations = maps:fold(FindStation, [], Monitor),

  case MatchingStations of
    [] -> {error, "No stations in this area"};
    _ ->
      Readings = lists:foldl(fun(StationKey, Acc) ->
          case maps:find(StationKey, Monitor) of
            {ok, Value} -> Value ++ Acc;
            error -> Acc
          end
        end, [], MatchingStations),

      FilteredReadings = lists:filter(fun({_, ExistingPollutant, _}) ->
          ExistingPollutant == Pollutant
        end, Readings),
      Values = lists:map(fun({_, _, Value}) -> Value end, FilteredReadings),
      case Values of
        [] ->
          {error, "No readings for this pollutant"};
        _ ->
          lists:sum(Values) / length(Values)
      end
  end.

