defmodule Creep.Packet.Subscribe do
  alias Creep.Packet.Subscribe

  defstruct [
    :packet_id,
    topic_filters: []
  ]

  defimpl Creep.Packet.Encode, for: Subscribe do
    @type_subscribe 0x08

    def encode(%Subscribe{}) do
      <<@type_subscribe::4, 0::1, 0::1, 1::1, 0::1, 0::8>>
    end
  end

  defimpl Creep.Packet.Decode, for: Subscribe do
    @type_subscribe 0x08

    def decode(%Subscribe{} = packet, <<
          @type_subscribe::4,
          # Bits 3,2,1 and 0 of the fixed header of the SUBSCRIBE Control Packet are reserved and MUST be set to 0,0,1 and 0 respectively. The Server MUST treat any other value as malformed and close the Network Connection [MQTT-3.8.1-1].
          # 0::1,
          # 0::1,
          # 1::1,
          # 0::1,
          _::4,
          payload_length::8,
          payload::binary-size(payload_length)
        >>) do
      <<packet_id::16, topic_filters::binary>> = payload
      topic_filters(%{packet | packet_id: packet_id}, topic_filters)
    end

    def topic_filters(
          packet,
          <<topic_filter_length::16, topic_filter::binary-size(topic_filter_length), 0::6, qos::2,
            rest::binary>>
        ) do
      topic_filters(%{packet | topic_filters: [{topic_filter, qos} | packet.topic_filters]}, rest)
    end

    def topic_filters(packet, <<>>) do
      %{packet | topic_filters: Enum.reverse(packet.topic_filters)}
    end
  end
end
