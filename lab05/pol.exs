dataList =
  File.read!("AirlyData-ALL-50k.csv")
  |> String.split("\n")
  |> Enum.filter(&(String.length(&1) > 0))


defmodule ParserLoader do
  def parse_line line do
    [dateTime,pollutionType,pollutionLevel,stationId,stationName,location_str] = line |> String.split(";")
    [date, time] = dateTime |> String.slice(0, 19) |> String.split("T")

    date = String.split(date, "-")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()

    time = String.split(time, ":")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()


    %{
      :location => location_str
      |> String.split(",")
      |> Enum.map(&String.to_float/1)
      |> List.to_tuple(),

      :stationId => stationId
      |> String.to_integer(),

      :dateTime => {date, time},

      :pollutionType => pollutionType,

      :pollutionLevel => pollutionLevel
      |> String.to_float(),

      :stationName => stationName

    }
  end
end

defmodule StationsManger do
  def identifyStations dataList do

    dataList
    |> Enum.filter(&(String.length(&1) > 0))
    |> Enum.map(&ParserLoader.parse_line/1)
    |> Enum.uniq_by(& &1.location)
    |> Enum.map(&["#{&1.stationId} #{&1.stationName}", &1.location])
  end

  def numberOfStations dataList do

    dataList
    |> Enum.filter(&(String.length(&1) > 0))
    |> Enum.map(&ParserLoader.parse_line/1)
    |> Enum.uniq_by(& &1.location)
    |> length

  end

  def dataReadyToAdd dataList do
    dataList
    |> Enum.filter(&(String.length(&1) > 0))
    |> Enum.map(&ParserLoader.parse_line/1)
  end
end

Code.append_path("/Users/Kamil/Documents/Studia/Semestr_4/erelix/borowka/_build/default/lib/borowka/ebin")
Application.start(:borowka)

stations = StationsManger.identifyStations(dataList)

# IO.inspect(stations)

{time, _result} = :timer.tc(fn ->
  stations
  |> Enum.each(fn [stationName, location] ->  :pollution_gen_server.addStation(stationName, location) end)
end)
IO.puts("Loading time: #{time} microseconds")
# StationsManger.dataReadyToAdd(dataList)
# |> Enum.each(fn data ->
#   :pollution_gen_server.addValue(data.location, data.dateTime, data.pollutionType, data.pollutionLevel) end)

# IO.inspect(:pollution_gen_server.getStationMean({50.11677, 19.965575}, "PM10"))


dataToInsert = StationsManger.dataReadyToAdd(dataList)


# {time, _result} = :timer.tc(fn ->
#   dataToInsert
#   |> Enum.each(fn data ->
#     if data.pollutionType == "PM10" do
#       :pollution_gen_server.addValue(data.location, data.dateTime, data.pollutionType, data.pollutionLevel)
#     end
#   end)
# end)

# IO.puts("Loading time: #{time} microseconds")


{time, _result} = :timer.tc(fn ->
  dataToInsert
  |> Enum.each(fn data ->
      :pollution_gen_server.addValue(data.location, data.dateTime, data.pollutionType, data.pollutionLevel)
  end)
end)

IO.puts("Loading time: #{time} microseconds")

{time, result} = :timer.tc(fn ->
  :pollution_gen_server.getStationMean("9910 Polska, Kraków, Studencka", "PM10")
end)

IO.puts("Station mean time: #{time} microseconds, result: #{result}")


{time, result} = :timer.tc(fn ->
  :pollution_gen_server.getDailyMean("PM25", {2024, 2, 10})
end)

IO.puts("Daily mean time: #{time} microseconds, result: #{result}")
