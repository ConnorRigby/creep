defmodule Creep do
  @moduledoc """
  Functions to start/stop a broker
  """

  require Logger
  use Supervisor

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
    transports = Keyword.fetch!(args, :transports)
    packet_processor = Keyword.fetch!(args, :packet_processor)
    Logger.info("Starting new Creep instance: #{broker_id}")

    transports =
      Enum.map(transports, fn
        {Creep.RanchTransport, transport_opts} ->
          {Creep.RanchTransport, Keyword.merge(args, transport_opts: transport_opts)}

        {Creep.PlugTransport, transport_opts} ->
          %{
            id: {Creep.PlugTransport, make_ref()},
            start:
              {Creep.PlugTransport, :start_link,
               [Keyword.merge(args, transport_opts: transport_opts)]}
          }
      end)

    children = [{packet_processor, args} | transports]
    Supervisor.init(children, strategy: :one_for_all)
  end
end
