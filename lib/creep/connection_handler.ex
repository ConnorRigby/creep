defmodule Creep.ConnectionHandler do
  @moduledoc """
  ranch protocol for handling tcp connections
  """
  alias Creep.{Packet, SessionRegistry}

  alias Packet.{
    Connect,
    Publish,
    Pubrel,
    Subscribe,
    Unsubscribe,
    Pingreq,
    Disconnect
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
  def init({ref, transport, protocol_opts}) do
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, [{:active, :once}])
    broker_id = Keyword.fetch!(protocol_opts, :broker_id)

    data = %{
      socket: socket,
      transport: transport,
      broker_id: broker_id,
      session: nil
    }

    :gen_statem.enter_loop(__MODULE__, [], :decode, data, [])
  end

  def decode(:info, {:tcp, socket, message}, data) do
    case Packet.decode(message) do
      {:ok, packet} ->
        actions = [{:next_event, :internal, {packet, socket}}]
        {:next_state, :process, data, actions}

      # Unless stated otherwise, if either the Server or Client encounters a 
      # protocol violation, it MUST close the Network Connection on which it 
      # received that Control Packet which caused the protocol violation [MQTT-4.8.0-1]. 
      {:error, reason} ->
        {:stop, reason, data}
    end
  end

  def decode(:info, %Publish{} = publish, data) do
    _ = reply(publish, data.socket, data)
    {:keep_state_and_data, []}
  end

  def decode(:info, {:tcp_closed, _socket}, data) do
    {:stop, :normal, data}
  end

  def process(:internal, {%Connect{} = connect, socket}, data) do
    _ = Logger.metadata(client_id: connect.client_id)
    :ok = data.transport.setopts(socket, [{:active, true}])
    {connack, session} = SessionRegistry.connect(data.broker_id, connect)
    reply(connack, socket, %{data | session: session})
  end

  def process(:internal, {%Publish{} = publish, socket}, data) do
    # TODO(Connor) Validate topic here somewhere 
    SessionRegistry.publish(data.broker_id, data.session, publish)
    |> reply(socket, data)
  end

  def process(:internal, {%Pubrel{} = pubrel, socket}, data) do
    SessionRegistry.pubrel(data.broker_id, data.session, pubrel)
    |> reply(socket, data)
  end

  def process(:internal, {%Subscribe{} = subscribe, socket}, data) do
    SessionRegistry.subscribe(data.broker_id, data.session, subscribe)
    |> reply(socket, data)
  end

  def process(:internal, {%Unsubscribe{} = unsubscribe, socket}, data) do
    SessionRegistry.unsubscribe(data.broker_id, data.session, unsubscribe)
    |> reply(socket, data)
  end

  def process(:internal, {%Pingreq{} = pingreq, socket}, data) do
    SessionRegistry.pingreq(data.broker_id, data.session, pingreq)
    |> reply(socket, data)
  end

  def process(:internal, {%Disconnect{} = disconnect, _socket}, data) do
    _ = SessionRegistry.disconnect(data.broker_id, data.session, disconnect)
    {:stop, :normal, data}
  end

  defp reply(nil, _socket, data) do
    {:next_state, :decode, data, []}
  end

  defp reply(reply, socket, data) do
    case Packet.encode(reply) do
      {:ok, packet} ->
        :ok = data.transport.send(socket, packet)
        {:next_state, :decode, data, []}

      {:error, reason} ->
        {:stop, reason, data}
    end
  end
end
