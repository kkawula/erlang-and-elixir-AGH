defmodule Pollutiondb.Reading do
  use Ecto.Schema

  schema "readings" do
    field :pollutionType, :string
    field :pollutionLevel, :float
    field :date, :date
    field :time, :time

    belongs_to :station, Pollutiondb.Station
  end

  def add_now(station, type, value) do
    reading = %Pollutiondb.Reading{station: Pollutiondb.Station.find_by_name(station), pollutionType: type, pollutionLevel: value, date: Date.utc_today, time: Time.truncate(Time.utc_now, :second)}
    reading
    |> Pollutiondb.Repo.insert
  end

  def add(station, type, value, date, time) do
    reading = %Pollutiondb.Reading{station: Pollutiondb.Station.find_by_name(station), pollutionType: type, pollutionLevel: value, date: Date.from_erl!(date), time: Time.from_erl!(time)}
    reading
    |> Pollutiondb.Repo.insert
  end

  def get_all do
    Pollutiondb.Repo.all(Pollutiondb.Reading)
  end

  def find_by_name(name) do
    station_id = Pollutiondb.Station.find_by_name(name).id
    Pollutiondb.Repo.all(Pollutiondb.Reading)
    |> Enum.filter(fn reading -> reading.station_id == station_id end)
  end

end
