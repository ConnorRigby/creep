defmodule Creep do
  @moduledoc """
  Functions to start/stop a broker
  """

  require Logger
  use Supervisor

  alias Creep.{
    InMemProcessor,
    RanchTransport
  }

  @type broker_id() :: term()
  @type transport_opts() :: Keyword.t()
  @type broker_opt() ::
          {:transport_opts, transport_opts()}
          | {:broker_id, broker_id()}

  @doc """
  Start a broker instance
  """
  @spec start_link([broker_opt()], GenServer.options()) :: GenServer.on_start()
  def start_link(args, opts \\ []) do
    Supervisor.start_link(__MODULE__, args, opts)
  end

  @impl Supervisor
  def init(args) do
    broker_id = Keyword.fetch!(args, :broker_id)
    Logger.info("Starting new Creep instance: #{broker_id}")

    args = Keyword.put_new(args, :packet_processor, InMemProcessor)
    args = Keyword.put_new(args, :packet_transport, RanchTransport)

    packet_processor = Keyword.fetch!(args, :packet_processor)
    packet_transport = Keyword.fetch!(args, :packet_transport)

    children = [
      {packet_processor, args},
      {packet_transport, args}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
