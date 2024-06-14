defmodule Pollutiondb.Repo.Migrations.CreateReadings do
  use Ecto.Migration

  def change do
    create table(:readings) do
      add :pollutionType, :string
      add :pollutionLevel, :float
      add :date, :date
      add :time, :time

      add :station_id, references(:stations, on_delete: :delete_all)
    end
  end
end
