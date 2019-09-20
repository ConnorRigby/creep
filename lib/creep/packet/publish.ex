defmodule Creep.Packet.Publish do
  alias Creep.Packet.Publish

  defstruct [
    :dup,
    :qos,
    :retain,
    :topic,
    :payload,
    :packet_id
  ]

  defimpl Creep.Packet.Encode, for: Publish do
    @type_publish 0x03

    def encode(%Publish{}) do
      <<@type_publish>>
    end
  end

  defimpl Creep.Packet.Decode, for: Publish do
    import Creep.Packet.Util
    @type_publish 0x03

    # QOS 1
    def decode(%Publish{} = packet, <<
          @type_publish::4,
          dup::1,
          1::2,
          retain::1,
          payload_size::8,
          payload::binary-size(payload_size)
        >>) do
      <<
        topic_size::8,
        topic::size(topic_size),
        packet_id::16,
        payload::binary
      >> = payload

      %{
        packet
        | dup: bool(dup),
          retain: bool(retain),
          qos: 1,
          topic: topic,
          payload: payload,
          packet_id: packet_id
      }
    end

    # QOS 0
    def decode(%Publish{} = packet, <<
          @type_publish::4,
          dup::1,
          0::2,
          retain::1,
          payload_size::8,
          payload::binary-size(payload_size)
        >>) do
      <<topic_size::8, topic::size(topic_size), payload::binary>> = payload
      %{packet | dup: bool(dup), retain: bool(retain), qos: 0, topic: topic, payload: payload}
    end
  end
end