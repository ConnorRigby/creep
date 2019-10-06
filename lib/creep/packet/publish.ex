defmodule Creep.Packet.Publish do
  alias Creep.Packet.Publish

  @type t() :: %Publish{
          dup: boolean(),
          qos: 0..2,
          retain: boolean(),
          topic: String.t(),
          payload: binary(),
          packet_id: nil | integer()
        }

  defstruct [
    :packet_id,
    :session_id,
    :dup,
    :qos,
    :retain,
    :topic,
    :payload
  ]

  def validate_topic!(topic) do
    Enum.each(Path.split(topic), fn
      "#" -> raise "Can not publish to wildcard topic"
      _ -> :ok
    end)

    :ok
  end

  defimpl Creep.Packet.Encode, for: Publish do
    import Creep.Packet.Util
    @type_publish 0x03

    def encode(%Publish{qos: 0} = publish) do
      IO.inspect(publish.payload, label: "PUBLISH.PAYLOAD")
      :ok = Publish.validate_topic!(publish.topic)
      topic_size = byte_size(publish.topic)

      payload = <<
        topic_size::16,
        publish.topic::binary-size(topic_size),
        publish.payload::binary
      >>

      <<
        @type_publish::4,
        bool(publish.dup)::1,
        0::2,
        bool(publish.retain)::1,
        byte_size(payload)::8
      >> <> payload
    end

    def encode(%Publish{qos: qos} = publish) when qos in [1, 2] do
      IO.inspect(publish.payload, label: "PUBLISH.PAYLOAD")
      :ok = Publish.validate_topic!(publish.topic)
      topic_size = byte_size(publish.topic)

      payload = <<
        topic_size::16,
        publish.topic::binary-size(topic_size),
        publish.packet_id::16,
        publish.payload::binary
      >>

      <<
        @type_publish::4,
        bool(publish.dup)::1,
        qos::2,
        bool(publish.retain)::1,
        byte_size(payload)::8
      >> <> payload
    end
  end

  defimpl Creep.Packet.Decode, for: Publish do
    import Creep.Packet.Util
    @type_publish 0x03

    # QOS 0
    def decode(%Publish{} = packet, <<
          @type_publish::4,
          dup::1,
          0::2,
          retain::1,
          payload_size::8,
          payload::binary-size(payload_size)
        >>) do
      <<topic_size::16, topic::binary-size(topic_size), payload::binary>> = payload
      :ok = Publish.validate_topic!(topic)
      %{packet | dup: bool(dup), retain: bool(retain), qos: 0, topic: topic, payload: payload}
    end

    # QOS 1
    def decode(%Publish{} = packet, <<
          @type_publish::4,
          dup::1,
          qos::2,
          retain::1,
          payload_size::8,
          payload::binary-size(payload_size)
        >>)
        when qos in [1, 2] do
      <<
        topic_size::16,
        topic::binary-size(topic_size),
        packet_id::16,
        payload::binary
      >> = payload

      :ok = Publish.validate_topic!(topic)

      %{
        packet
        | dup: bool(dup),
          retain: bool(retain),
          qos: qos,
          topic: topic,
          payload: payload,
          packet_id: packet_id
      }
    end
  end
end
