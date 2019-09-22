defmodule Creep.PlugTransport do
  require Logger
  @behaviour Creep.PacketTransport

  use Supervisor
  alias Creep.PlugTransport.{MQTTSocket, MQTTRouter}

  def child_spec(opts) do
    broker_id = Keyword.fetch!(opts, :broker_id)

    %{
      id: {__MODULE__, broker_id},
      start: {__MODULE__, :start_link, opts}
    }
  end

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts)
  end

  def init(opts) do
    Logger.info("Starting Websocket MQTT Broker")
    transport_opts = Keyword.get(opts, :transport_opts, [])
    _ = Keyword.fetch!(transport_opts, :port)
    {scheme, transport_opts} = Keyword.pop(transport_opts, :scheme, :http)

    plug_opts = Keyword.merge(transport_opts, dispatch: dispatch(opts))

    children = [
      {Plug.Adapters.Cowboy, scheme: scheme, plug: Router, options: plug_opts}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp dispatch(args) do
    [
      {:_,
       [
         {"/mqtt", MQTTSocket, args},
         {:_, Plug.Cowboy.Handler, {MQTTRouter, []}}
       ]}
    ]
  end
end
