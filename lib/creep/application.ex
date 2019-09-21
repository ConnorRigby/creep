defmodule Creep.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    brokers = Application.get_env(:creep, __MODULE__)[:brokers]

    children =
      for opts <- brokers do
        {Creep, opts}
      end

    opts = [strategy: :one_for_one, name: Creep.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
