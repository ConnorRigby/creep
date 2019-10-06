defmodule Creep.Packet do
  @moduledoc """
  Functions for encoding/decoding MQTT packets
  """

  alias Creep.Packet

  alias Creep.Packet.{
    Connect,
    Connack,
    Publish,
    Puback,
    Pubrec,
    Pubrel,
    Pubcomp,
    Subscribe,
    Suback,
    Unsubscribe,
    Unsuback,
    Pingreq,
    Pingresp,
    Disconnect
  }

  defprotocol Encode do
    @moduledoc false
    @doc "Encode data to a binary"
    def encode(data)
  end

  defprotocol Decode do
    @moduledoc false
    @doc "Decode data to a struct"
    def decode(data, packet)
  end

  # packet types
  @type_connect 0x01
  @type_connack 0x02
  @type_publish 0x03
  @type_puback 0x04
  @type_pubrec 0x05
  @type_pubrel 0x06
  @type_pubcomp 0x07
  @type_subscribe 0x08
  @type_suback 0x09
  @type_unsubscribe 0xA
  @type_unsuback 0xB
  @type_pingreq 0xC
  @type_pingresp 0xD
  @type_disconnect 0xE

  @typedoc "struct describing an MQTT packet"
  @type t() ::
          Connect.t()
          | Connack.t()
          | Publish.t()
          | Puback.t()
          | Pubrec.t()
          | Pubrel.t()
          | Pubcomp.t()
          | Subscribe.t()
          | Suback.t()
          | Unsubscribe.t()
          | Unsuback.t()
          | Pingreq.t()
          | Pingresp.t()
          | Disconnect.t()

  @doc "Decode an MQTT packet"
  @spec decode(binary()) :: {:ok, Packet.t()} | {:error, term()}
  def decode(binary) do
    try do
      case decode_dispatch(binary) do
        {:error, reason} -> {:error, reason}
        decoded -> {:ok, decoded}
      end
    catch
      type, error ->
        IO.inspect(error, label: "#{type} decode")
        {:error, :invalid_packet}
    end
  end

  @doc "Encode a packet back to a binary"
  @spec encode(Packet.t()) :: {:ok, binary()} | {:error, term()}
  def encode(%_type{} = data) do
    try do
      case Encode.encode(data) do
        encoded when is_binary(encoded) -> {:ok, encoded}
        {:error, reason} -> {:error, reason}
      end
    catch
      type, error ->
        IO.puts("Failed to encode packet: #{inspect(data)}")
        IO.inspect(error, label: "#{type} encode")
        {:error, :invalid_packet}
    end
  end

  defp decode_dispatch(<<@type_connect::4, _::4, _::binary>> = connect),
    do: Decode.decode(%Connect{}, connect)

  defp decode_dispatch(<<@type_connack::4, _::4, _::binary>> = connack),
    do: Decode.decode(%Connack{}, connack)

  defp decode_dispatch(<<@type_publish::4, _::4, _::binary>> = publish),
    do: Decode.decode(%Publish{}, publish)

  defp decode_dispatch(<<@type_puback::4, _::4, _::binary>> = puback),
    do: Decode.decode(%Puback{}, puback)

  defp decode_dispatch(<<@type_pubrec::4, _::4, _::binary>> = pubrec),
    do: Decode.decode(%Pubrec{}, pubrec)

  defp decode_dispatch(<<@type_pubrel::4, _::4, _::binary>> = pubrel),
    do: Decode.decode(%Pubrel{}, pubrel)

  defp decode_dispatch(<<@type_pubcomp::4, _::4, _::binary>> = pubcomp),
    do: Decode.decode(%Pubcomp{}, pubcomp)

  defp decode_dispatch(<<@type_subscribe::4, _::4, _::binary>> = subscribe),
    do: Decode.decode(%Subscribe{}, subscribe)

  defp decode_dispatch(<<@type_suback::4, _::4, _::binary>> = suback),
    do: Decode.decode(%Suback{}, suback)

  defp decode_dispatch(<<@type_unsubscribe::4, _::4, _::binary>> = unsubscribe),
    do: Decode.decode(%Unsubscribe{}, unsubscribe)

  defp decode_dispatch(<<@type_unsuback::4, _::4, _::binary>> = unsuback),
    do: Decode.decode(%Unsuback{}, unsuback)

  defp decode_dispatch(<<@type_pingreq::4, _::4, _::binary>> = pingreq),
    do: Decode.decode(%Pingreq{}, pingreq)

  defp decode_dispatch(<<@type_pingresp::4, _::4, _::binary>> = pingresp),
    do: Decode.decode(%Pingresp{}, pingresp)

  defp decode_dispatch(<<@type_disconnect::4, _::4, _::binary>> = disconnect),
    do: Decode.decode(%Disconnect{}, disconnect)

  defp decode_dispatch(unknown), do: {:error, {:unknown, unknown}}
end
