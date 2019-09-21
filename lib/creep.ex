defmodule Creep do
  @moduledoc """
  Functions to start/stop a broker
  """

  use Supervisor

  alias Creep.{
    ConnectionHandler
  }

  @type broker_port() :: integer()
  @type broker_opt() :: broker_port()

  @doc """
  Start a broker instance
  """
  @spec start_link([broker_opt()], GenServer.options()) :: GenServer.on_start()
  def start_link(args, opts \\ []) do
    Supervisor.start_link(__MODULE__, args, opts)
  end

  @impl Supervisor
  def init(args) do
    port = Keyword.fetch!(args, :port)

    children = [
      {ConectionTracker, args},
      :ranch.child_spec(make_ref(), :ranch_tcp, [{:port, port}], ConnectionHandler, [])
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  def connection_test do
    Tortoise.Supervisor.start_child(
      client_id: "my_client_id",
      handler: {Tortoise.Handler.Logger, []},
      server: {Tortoise.Transport.Tcp, host: 'localhost', port: 1883},
      subscriptions: [{"a/b", 2}, {"c/d", 0}]
    )
  end

  def publish_test do
    Tortoise.publish("my_client_id", "foo/bar", "Hello from the World of Tomorrow !", qos: 2)
  end
end
