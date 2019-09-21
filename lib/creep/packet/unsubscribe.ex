defmodule Creep.Packet.Unsubscribe do
  alias Creep.Packet.Unsubscribe

  @type t() :: %Unsubscribe{
          packet_id: integer(),
          topic_filters: [String.t()]
        }

  defstruct [
    :packet_id,
    topic_filters: []
  ]

  defimpl Creep.Packet.Encode, for: Unsubscribe do
    @type_unsubscribe 0xA

    def encode(%Unsubscribe{}) do
      <<@type_unsubscribe::4>>
    end
  end

  defimpl Creep.Packet.Decode, for: Unsubscribe do
    @type_unsubscribe 0xA

    def decode(%Unsubscribe{} = packet, <<
          @type_unsubscribe::4,
          # Bits 3,2,1 and 0 of the fixed header of the SUBSCRIBE Control Packet are reserved and MUST be set to 0,0,1 and 0 respectively. The Server MUST treat any other value as malformed and close the Network Connection [MQTT-3.8.1-1].
          0::1,
          0::1,
          1::1,
          0::1,
          payload_length::8,
          payload::binary-size(payload_length)
        >>) do
      <<packet_id::16, topic_filters::binary>> = payload
      topic_filters(%{packet | packet_id: packet_id}, topic_filters)
    end

    def topic_filters(
          packet,
          <<topic_filter_length::16, topic_filter::binary-size(topic_filter_length),
            rest::binary>>
        ) do
      topic_filters(%{packet | topic_filters: [topic_filter | packet.topic_filters]}, rest)
    end

    def topic_filters(packet, <<>>) do
      %{packet | topic_filters: Enum.reverse(packet.topic_filters)}
    end
  end
end
