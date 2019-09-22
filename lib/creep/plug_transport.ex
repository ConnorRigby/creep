defmodule Creep.PlugTransport do
  @behaviour Creep.PacketTransport

  use Supervisor
  alias Creep.PlugTransport.{MQTTSocket, MQTTRouter}

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts)
  end

  def init(opts) do
    transport_opts = Keyword.get(opts, :transport_opts, [])
    port = Keyword.fetch!(transport_opts, :port)
    scheme = Keyword.get(transport_opts, :scheme, :http)

    plug_opts = [
      port: port,
      dispatch: dispatch(opts)
    ]

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
