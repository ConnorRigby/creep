defmodule Creep.Packet do
  alias Creep.Packet.{
    Connect,
    # Connack,
    Publish
    # Puback
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
  # @type_connack 0x02
  @type_publish 0x03
  # @type_puback 0x04

  def decode(<<@type_connect::4, _::4, _::binary>> = connect) do
    {:ok, Decode.decode(%Connect{}, connect)}
  end

  def decode(<<@type_publish::4, _::4, _::binary>> = publish) do
    {:ok, Decode.decode(%Publish{}, publish)}
  end

  def decode(unknown), do: {:error, {:unknown, unknown}}

  def encode(%_type{} = data) do
    {:ok, Encode.encode(data)}
  end
end
