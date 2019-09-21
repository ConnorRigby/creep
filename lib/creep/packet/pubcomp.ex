defmodule Creep.Packet.Pubcomp do
  alias Creep.Packet.Pubcomp

  defstruct [
    :packet_id
  ]

  defimpl Creep.Packet.Encode, for: Pubcomp do
    @type_pubcomp 0x07

    def encode(%Pubcomp{packet_id: packet_id}) do
      <<@type_pubcomp::4, 0::4, 2::8, packet_id::16>>
    end
  end

  defimpl Creep.Packet.Decode, for: Pubcomp do
    @type_pubcomp 0x07

    def decode(%Pubcomp{} = packet, <<
          @type_pubcomp::4,
          _reserved::4,
          2::8,
          packet_id::16
        >>) do
      %{packet | packet_id: packet_id}
    end
  end
end
