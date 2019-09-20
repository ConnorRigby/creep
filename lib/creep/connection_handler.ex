defmodule Creep.ConnectionHandler do
  alias Creep.Packet

  alias Packet.{
    Connect,
    Connack,
    Publish,
    Puback
  }

  @behaviour :ranch_protocol
  @behaviour :gen_statem

  @impl :ranch_protocol
  def start_link(ref, transport, protocol_opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, protocol_opts}])}
  end

  @impl :gen_statem
  def callback_mode() do
    :state_functions
  end

  @impl :gen_statem
  def init({ref, transport, _protocol_opts}) do
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, [{:active, :once}])

    data = %{
      socket: socket,
      transport: transport
    }

    :gen_statem.enter_loop(__MODULE__, [], :decode, data, [])
  end

  def decode(:info, {:tcp, socket, message}, data) do
    case Packet.decode(message) do
      {:ok, packet} ->
        actions = [{:next_event, :internal, {packet, socket}}]
        {:next_state, :process, data, actions}

      {:error, reason} ->
        {:stop, reason, data}
    end
  end

  def process(:internal, {%Connect{keep_alive: keep_alive}, socket}, data) do
    connack = %Connack{}

    case Packet.encode(connack) do
      {:ok, packet} ->
        :ok = data.transport.setopts(socket, [{:active, true}])
        :ok = data.transport.send(socket, packet)
        Process.send_after(self(), :keep_alive, keep_alive * 1000)
        {:next_state, :decode, data, []}

      {:error, reason} ->
        {:stop, reason, data}
    end
  end

  # QOS 0 doesn't require acking
  def process(:internal, {%Publish{qos: 0}, _socket}, data) do
    {:next_state, :decode, data, []}
  end

  def process(:internal, {%Publish{qos: 1, packet_id: packet_id}, socket}, data) do
    puback = %Puback{packet_id: packet_id}

    case Packet.encode(puback) do
      {:ok, packet} ->
        :ok = data.transport.send(socket, packet)
        {:next_state, :decode, data, []}

      {:error, reason} ->
        {:stop, reason, data}
    end
  end
end
