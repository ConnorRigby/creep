defmodule Creep.PlugTransport.MQTTSocket do
  require Logger

  alias Creep.Packet

  alias Packet.{
    Connect,
    Publish,
    Pubrel,
    Subscribe,
    Unsubscribe,
    Pingreq,
    Disconnect
  }

  @behaviour :cowboy_websocket
  def init(req, state) do
    {:cowboy_websocket, req, state}
  end

  def websocket_init(protocol_opts) do
    broker_id = Keyword.fetch!(protocol_opts, :broker_id)
    packet_processor = Keyword.fetch!(protocol_opts, :packet_processor)

    data = %{
      broker_id: broker_id,
      packet_processor: packet_processor,
      session: nil
    }

    {:ok, data}
  end

  def websocket_handle({:binary, message}, data) do
    case Packet.decode(message) do
      {:ok, packet} ->
        process(packet, data)

      # Unless stated otherwise, if either the Server or Client encounters a 
      # protocol violation, it MUST close the Network Connection on which it 
      # received that Control Packet which caused the protocol violation [MQTT-4.8.0-1]. 
      {:error, reason} ->
        {:stop, reason, data}
    end
  end

  def websocket_handle({:text, message}, state) do
    Logger.info("Dropping message: #{inspect(message)}")
    {:stop, {:unhandled_message, message}, state}
  end

  def websocket_handle({:ping, "PING"}, state) do
    {:reply, {:ping, "PONG"}, state}
  end

  def websocket_info(%Publish{} = publish, data) do
    reply(publish, data)
  end

  def websocket_info(info, state) do
    Logger.info("Dropping #{inspect(info)}")
    {:ok, state}
  end

  def process(%Connect{} = connect, data) do
    _ = Logger.metadata(client_id: connect.client_id)
    {connack, session} = data.packet_processor.connect(data.broker_id, connect)
    reply(connack, %{data | session: session})
  end

  def process(%Publish{} = publish, data) do
    # TODO(Connor) Validate topic here somewhere 
    data.packet_processor.publish(data.broker_id, data.session, publish)
    |> reply(data)
  end

  def process(%Pubrel{} = pubrel, data) do
    data.packet_processor.pubrel(data.broker_id, data.session, pubrel)
    |> reply(data)
  end

  def process(%Subscribe{} = subscribe, data) do
    data.packet_processor.subscribe(data.broker_id, data.session, subscribe)
    |> reply(data)
  end

  def process(%Unsubscribe{} = unsubscribe, data) do
    data.packet_processor.unsubscribe(data.broker_id, data.session, unsubscribe)
    |> reply(data)
  end

  def process(%Pingreq{} = pingreq, data) do
    data.packet_processor.pingreq(data.broker_id, data.session, pingreq)
    |> reply(data)
  end

  def process(%Disconnect{} = disconnect, data) do
    _ = data.packet_processor.disconnect(data.broker_id, data.session, disconnect)
    {:stop, :normal, data}
  end

  def process(packet, data) do
    {:stop, {:unhandled_packet, packet}, data}
  end

  def terminate(_reason, _req, _data) do
    :ok
  end

  defp reply(nil, data), do: {:ok, data}

  defp reply(packet, data) do
    case Packet.encode(packet) do
      {:ok, packet} ->
        {:reply, [{:binary, packet}], data}

      {:error, reason} ->
        {:stop, reason, data}
    end
  end
end
