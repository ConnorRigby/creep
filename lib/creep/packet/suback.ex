defmodule Creep.Packet.Suback do
  alias Creep.Packet.Suback

  @type t() :: %Suback{
          packet_id: integer(),
          responses: [0..2 | 0x80]
        }

  defstruct [
    :packet_id,
    responses: []
  ]

  defimpl Creep.Packet.Encode, for: Suback do
    @type_suback 0x09

    def encode(%Suback{packet_id: packet_id, responses: responses}) do
      size = 2 + Enum.count(responses)
      encode_responses(responses, <<@type_suback::4, 0::4, size::8, packet_id::16>>)
    end

    def encode_responses([response | rest], data) do
      encode_responses(rest, data <> <<response::8>>)
    end

    def encode_responses([], data) do
      data
    end
  end

  defimpl Creep.Packet.Decode, for: Suback do
    @type_suback 0x09

    def decode(%Suback{} = packet, <<
          @type_suback::4,
          0::4,
          payload_length::8,
          payload::binary-size(payload_length)
        >>) do
      <<packet_id::16>> = payload
      %{packet | packet_id: packet_id}
    end
  end
end
