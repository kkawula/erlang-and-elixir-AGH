defmodule Pollutiondb.Station do
  use Ecto.Schema
  require Ecto.Query

  schema "stations" do
      field :name, :string
      field :lon, :float
      field :lat, :float

      has_many :readings, Pollutiondb.Reading
    end

  def insertMockData() do
    [
      %Pollutiondb.Station{name: "Aleja Słowackiego", lon: 1.1, lat: 1.1,},
      %Pollutiondb.Station{name: "Nowa Huta", lon: 2.1, lat: 2.1},
      %Pollutiondb.Station{name: "Krowodrza Górka", lon: 3.1, lat: 3.1},
      %Pollutiondb.Station{name: "Prądnik Biały", lon: 4.1, lat: 4.1}
    ]
    |> Enum.map(fn station -> station |> add() end)

    :ok
  end
  def update_name(station, newname) do
    station
    |> chs(%{name: newname})
    |> Pollutiondb.Repo.update
  end

  defp chs(station, changesmap) do
    station
    |> Ecto.Changeset.cast(changesmap, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name])
    |> Ecto.Changeset.validate_length(:name, min: 4)
    |> Ecto.Changeset.validate_number(:lon, less_than: 180, greater_than: -180)
    |> Ecto.Changeset.validate_number(:lat, less_than: 90, greater_than: -90)
  end

  def add(station) do
    station
    |> chs(%{name: station.name, lon: station.lon, lat: station.lat})
    |> Pollutiondb.Repo.insert
  end

  def add(name, lon, lat) do
    station = %Pollutiondb.Station{name: name, lon: lon, lat: lat}
    station
    |> chs(%{name: name, lon: lon, lat: lat})
    |> Pollutiondb.Repo.insert
  end

  def get_all do
    Pollutiondb.Repo.all(Pollutiondb.Station)
  end


  def get_by_id(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
  end

  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end

  def find_by_name(name) do
    Ecto.Query.where(Pollutiondb.Station, name: ^name) |> Pollutiondb.Repo.one
  end

  def find_by_name_many(name) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.name == ^name)
      |> Pollutiondb.Repo.all
  end

  def find_by_location(lon, lat) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat)
      |> Pollutiondb.Repo.all
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon >= ^lon_min,
      where: s.lon <= ^lon_max,
      where: s.lat >= ^lat_min,
      where: s.lat <= ^lat_max)
      |> Pollutiondb.Repo.all
  end
end

# stations |> Enum.each(fn station -> Pollutiondb.Station.add(station) end)
