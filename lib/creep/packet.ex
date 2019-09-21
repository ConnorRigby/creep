defmodule Creep.Packet do
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
    @doc "Encode data to a binary"
    def encode(data)
  end

  defprotocol Decode do
    @doc "Decode data to a struct"
    def decode(data, packet)
  end

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

  def decode(<<@type_connect::4, _::4, _::binary>> = connect) do
    {:ok, Decode.decode(%Connect{}, connect)}
  end

  def decode(<<@type_connack::4, _::4, _::binary>> = connack) do
    {:ok, Decode.decode(%Connack{}, connack)}
  end

  def decode(<<@type_publish::4, _::4, _::binary>> = publish) do
    {:ok, Decode.decode(%Publish{}, publish)}
  end

  def decode(<<@type_puback::4, _::4, _::binary>> = puback) do
    {:ok, Decode.decode(%Puback{}, puback)}
  end

  def decode(<<@type_pubrec::4, _::4, _::binary>> = pubrec) do
    {:ok, Decode.decode(%Pubrec{}, pubrec)}
  end

  def decode(<<@type_pubrel::4, _::4, _::binary>> = pubrel) do
    {:ok, Decode.decode(%Pubrel{}, pubrel)}
  end

  def decode(<<@type_pubcomp::4, _::4, _::binary>> = pubcomp) do
    {:ok, Decode.decode(%Pubcomp{}, pubcomp)}
  end

  def decode(<<@type_subscribe::4, _::4, _::binary>> = subscribe) do
    {:ok, Decode.decode(%Subscribe{}, subscribe)}
  end

  def decode(<<@type_suback::4, _::4, _::binary>> = suback) do
    {:ok, Decode.decode(%Suback{}, suback)}
  end

  def decode(<<@type_unsubscribe::4, _::4, _::binary>> = unsubscribe) do
    {:ok, Decode.decode(%Unsubscribe{}, unsubscribe)}
  end

  def decode(<<@type_unsuback::4, _::4, _::binary>> = unsuback) do
    {:ok, Decode.decode(%Unsuback{}, unsuback)}
  end

  def decode(<<@type_pingreq::4, _::4, _::binary>> = pingreq) do
    {:ok, Decode.decode(%Pingreq{}, pingreq)}
  end

  def decode(<<@type_pingresp::4, _::4, _::binary>> = pingresp) do
    {:ok, Decode.decode(%Pingresp{}, pingresp)}
  end

  def decode(<<@type_disconnect::4, _::4, _::binary>> = disconnect) do
    {:ok, Decode.decode(%Disconnect{}, disconnect)}
  end

  def decode(unknown), do: {:error, {:unknown, unknown}}

  def encode(%_type{} = data) do
    {:ok, Encode.encode(data)}
  end
end
