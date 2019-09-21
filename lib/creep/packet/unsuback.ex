defmodule Creep.Packet.Unsuback do
  alias Creep.Packet.Unsuback

  defstruct [
    :packet_id
  ]

  defimpl Creep.Packet.Encode, for: Unsuback do
    @type_unsuback 0xB

    def encode(%Unsuback{packet_id: packet_id}) do
      <<@type_unsuback::4, 0::4, 2::8, packet_id::16>>
    end
  end

  defimpl Creep.Packet.Decode, for: Unsuback do
    @type_unsuback 0xB

    def decode(%Unsuback{} = packet, <<
          @type_unsuback::4,
          _reserved::4,
          2::8,
          packet_id::16
        >>) do
      %{packet | packet_id: packet_id}
    end
  end
end
