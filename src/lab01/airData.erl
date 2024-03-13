%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2024 8:19 PM
%%%-------------------------------------------------------------------
-module(airData).
-author("Kamil").

%% API
-export([number_of_readings/2, calculate_max/2, calculate_mean/2, get_readings/0]).

%%A = {"nazwa stacji", {date(), time()}, [{"pm10", 123}, {"pm5", 99}]}.
get_readings() ->
  [
    {"Station1", {{2024, 2, 29}, {20, 41, 18}}, [{"pm10", 123}, {"pm5", 99}]},
    {"Station2", {{2024, 2, 29}, {18, 15, 39}}, [{"pm10", 234}, {"pm5", 198}]},
    {"Station1", {{2024, 3, 1}, {12, 12, 59}}, [{"pm10", 345}, {"pm5", 297}]}
  ].
number_of_readings(Readings, Date) ->
  FilteredReadings = lists:filter(fun({_, {DateInTuple, _}, _}) when DateInTuple == Date -> true; (_) -> false end, Readings),
  case FilteredReadings of
    [] ->"No data for this type of reading.";
    _ -> length(FilteredReadings)
  end.

calculate_max(Readings, Type) ->
  FilteredReadings = lists:filter(fun({_, _, Data}) -> lists:keyfind(Type, 1, Data) /= false end, Readings),
  Values = lists:map(fun({_, _, Data}) -> {Type, Value} = lists:keyfind(Type, 1, Data), Value end, FilteredReadings),
  case Values of
    [] -> "No data for this type of reading.";
    _ -> lists:max(Values)
  end.

calculate_mean(Readings, Type) ->
  FilteredReadings = lists:filter(fun({_, _, Data}) -> lists:keyfind(Type, 1, Data) /= false end, Readings),
  Values = lists:map(fun({_, _, Data}) -> {Type, Value} = lists:keyfind(Type, 1, Data), Value end, FilteredReadings),
  case Values of
    [] -> "No data for this type of reading.";
    _ -> lists:sum(Values) / length(Values)
  end.