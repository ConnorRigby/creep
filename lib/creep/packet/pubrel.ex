defmodule Creep.Packet.Pubrel do
  alias Creep.Packet.Pubrel

  @type t() :: %Pubrel{
          packet_id: integer()
        }

  defstruct [
    :packet_id
  ]

  defimpl Creep.Packet.Encode, for: Pubrel do
    @type_pubrel 0x06

    def encode(%Pubrel{packet_id: packet_id}) do
      <<@type_pubrel::4, 0::1, 0::1, 1::1, 0::1, 2::8, packet_id::16>>
    end
  end

  defimpl Creep.Packet.Decode, for: Pubrel do
    @type_pubrel 0x06

    def decode(%Pubrel{} = packet, <<
          @type_pubrel::4,
          _reserved::4,
          2::8,
          packet_id::16
        >>) do
      %{packet | packet_id: packet_id}
    end
  end
end
