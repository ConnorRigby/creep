defmodule Creep.ConnectionTracker do
  @moduledoc """
  Tracks all connections to a broker
  """

  use GenServer
  alias __MODULE__, as: State
  defstruct []

  @doc false
  @spec start_link(Creep.broker_opts(), GenServer.options()) :: GenServer.on_start()
  def start_link(args, opts \\ []) do
    GenServer.start_link(__MODULE__, args, opts)
  end

  @impl GenServer
  def init(_args) do
    {:ok, %State{}}
  end
end
