defmodule Creep do
  @moduledoc """
  Functions to start/stop a broker
  """

  require Logger
  use Supervisor

  alias Creep.{
    SessionRegistry,
    ConnectionHandler
  }

  @type broker_port() :: integer()
  @type broker_id() :: term()
  @type broker_opt() :: broker_port() | broker_id()

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
    port = Keyword.fetch!(args, :port)

    children = [
      {SessionRegistry, args},
      :ranch.child_spec(make_ref(), :ranch_tcp, [{:port, port}], ConnectionHandler, args)
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  def connection_test(client_id \\ "my_client_id") do
    Tortoise.Supervisor.start_child(
      client_id: client_id,
      handler: {Tortoise.Handler.Logger, []},
      server: {Tortoise.Transport.Tcp, host: 'localhost', port: 1883},
      subscriptions: [{"a/#", 0}]
    )
  end

  def publish_test(client_id \\ "my_client_id") do
    Tortoise.publish(client_id, "a/b", "Hello from the World of Tomorrow !", qos: 0)
  end
end
