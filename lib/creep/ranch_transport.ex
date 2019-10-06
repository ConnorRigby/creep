defmodule Creep.RanchTransport do
  @moduledoc """
  ranch protocol for handling tcp connections
  """
  require Logger

  alias Creep.Packet

  alias Packet.{
    Connect,
    Publish,
    Puback,
    Pubrel,
    Subscribe,
    Unsubscribe,
    Pingreq,
    Disconnect
  }

  # Ranch 1.7 does not allow this unfortunately
  # @behaviour Creep.PacketTransport

  @behaviour :ranch_protocol
  @behaviour :gen_statem

  # Ranch 1.7 does not allow this unfortunately
  # @impl Creep.PacketTransport
  @doc false
  def child_spec(opts) do
    transport_opts = Keyword.get(opts, :transport_opts, [])
    {ssl?, transport_opts} = Keyword.pop(transport_opts, :ssl, false)

    if ssl? do
      :ranch.child_spec(make_ref(), :ranch_ssl, transport_opts, __MODULE__, opts)
    else
      :ranch.child_spec(make_ref(), :ranch_tcp, transport_opts, __MODULE__, opts)
    end
  end

  @impl :ranch_protocol
  def start_link(ref, _socket, transport, protocol_opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, protocol_opts}])}
  end

  # @impl :ranch_protocol
  # compat for ranch 2.x
  def start_link(ref, transport, protocol_opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, protocol_opts}])}
  end

  @impl :gen_statem
  def callback_mode() do
    :state_functions
  end

  @impl :gen_statem
  def init({ref, transport, protocol_opts}) do
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, [{:active, :once}])
    broker_id = Keyword.fetch!(protocol_opts, :broker_id)
    packet_processor = Keyword.fetch!(protocol_opts, :packet_processor)

    data = %{
      socket: socket,
      transport: transport,
      broker_id: broker_id,
      packet_processor: packet_processor,
      session: nil,
      transport_type: :tcp
    }

    :gen_statem.enter_loop(__MODULE__, [], :decode, data, [])
  end

  @impl :gen_statem
  def terminate(reason, _state, data) do
    data.packet_processor.disconnect(data.broker_id, data.session, %Disconnect{reason: reason})
  end

  def decode(:info, {type, socket, message}, data) when type in [:tcp, :ssl] do
    Logger.info("Received packet: #{inspect(message, base: :hex)}")

    case Packet.decode(message) do
      {:ok, packet} ->
        actions = [{:next_event, :internal, {packet, socket}}]
        {:next_state, :process, %{data | transport_type: type}, actions}

      # Unless stated otherwise, if either the Server or Client encounters a 
      # protocol violation, it MUST close the Network Connection on which it 
      # received that Control Packet which caused the protocol violation [MQTT-4.8.0-1]. 
      {:error, reason} ->
        {:stop, reason, data}
    end
  end

  def decode(:info, %Publish{} = publish, data) do
    reply({publish, data.session}, data.socket, data)
  end

  def decode(:info, %Disconnect{reason: reason}, data) do
    {:stop, reason, data}
  end

  def decode(:info, {:tcp_closed, _socket}, data) do
    {:stop, :normal, data}
  end

  def process(:internal, {%Connect{} = connect, socket}, data) do
    _ = Logger.metadata(client_id: connect.client_id)
    Logger.info("New TCP Broker connection")
    :ok = data.transport.setopts(socket, [{:active, true}])

    data.packet_processor.connect(data.broker_id, connect)
    |> reply(socket, data)
  end

  def process(:internal, {%Publish{} = publish, socket}, data) do
    data.packet_processor.publish(data.broker_id, data.session, publish)
    |> reply(socket, data)
  end

  def process(:internal, {%Puback{} = puback, socket}, data) do
    data.packet_processor.puback(data.broker_id, data.session, puback)
    |> reply(socket, data)
  end

  def process(:internal, {%Pubrel{} = pubrel, socket}, data) do
    data.packet_processor.pubrel(data.broker_id, data.session, pubrel)
    |> reply(socket, data)
  end

  def process(:internal, {%Subscribe{} = subscribe, socket}, data) do
    Logger.info("Subscribe #{inspect(subscribe.topic_filters)}")

    data.packet_processor.subscribe(data.broker_id, data.session, subscribe)
    |> reply(socket, data)
  end

  def process(:internal, {%Unsubscribe{} = unsubscribe, socket}, data) do
    data.packet_processor.unsubscribe(data.broker_id, data.session, unsubscribe)
    |> reply(socket, data)
  end

  def process(:internal, {%Pingreq{} = pingreq, socket}, data) do
    data.packet_processor.pingreq(data.broker_id, data.session, pingreq)
    |> reply(socket, data)
  end

  def process(:internal, {%Disconnect{reason: reason} = disconnect, _socket}, data) do
    _ = data.packet_processor.disconnect(data.broker_id, data.session, disconnect)
    {:stop, reason, data}
  end

  defp reply({nil, session}, _socket, data) do
    {:next_state, :decode, %{data | session: session}, []}
  end

  defp reply({reply, session}, socket, data) do
    case Packet.encode(reply) do
      {:ok, packet} ->
        :ok = data.transport.send(socket, packet)
        {:next_state, :decode, %{data | session: session}, []}

      {:error, reason} ->
        {:stop, reason, %{data | session: session}}
    end
  end
end
