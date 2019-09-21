defmodule Creep.Packet.Puback do
  alias Creep.Packet.Puback

  @type t() :: %Puback{
          packet_id: integer()
        }

  defstruct [
    :packet_id
  ]

  defimpl Creep.Packet.Encode, for: Puback do
    @type_puback 0x04

    def encode(%Puback{packet_id: packet_id}) do
      <<@type_puback::4, 0::4, 2::8, packet_id::16>>
    end
  end

  defimpl Creep.Packet.Decode, for: Puback do
    @type_puback 0x04

    def decode(%Puback{} = packet, <<
          @type_puback::4,
          _reserved::4,
          2::8,
          packet_id::16
        >>) do
      %{packet | packet_id: packet_id}
    end
  end
end
