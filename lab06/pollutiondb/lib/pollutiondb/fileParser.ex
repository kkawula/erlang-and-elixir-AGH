defmodule Pollutiondb.FileParser do
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

  def identifyStations dataList do

    dataList
    |> Enum.filter(&(String.length(&1) > 0))
    |> Enum.map(&Pollutiondb.FileParser.parse_line/1)
    |> Enum.uniq_by(& &1.location)
    |> Enum.map(&["#{&1.stationId} #{&1.stationName}", &1.location])
  end

  def dataReadyToAdd dataList do
    dataList
    |> Enum.filter(&(String.length(&1) > 0))
    |> Enum.map(&Pollutiondb.FileParser.parse_line/1)
  end

  def addStations do
    dataList = File.read!("lib/data/AirlyData-ALL-50k.csv")
    |> String.split("\n")
    |> Enum.filter(&(String.length(&1) > 0))

    stations = Pollutiondb.FileParser.identifyStations(dataList)

    stations
    |> Enum.each(
      fn data ->
        [name, {lan, lat}] = data
        Pollutiondb.Station.add(name, lan, lat)
      end
    )
  end

  def addReadings do
    dataList = File.read!("lib/data/AirlyData-ALL-50k.csv")
    |> String.split("\n")
    |> Enum.filter(&(String.length(&1) > 0))

    dataToAdd = Pollutiondb.FileParser.dataReadyToAdd(dataList)

    dataToAdd
    |> Enum.each(
      fn data ->
        {date, time} = data.dateTime
        Pollutiondb.Reading.add(data.stationName, data.pollutionType, data.pollutionLevel, date, time)
      end
    )
  end
end
