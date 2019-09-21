defmodule Creep.Packet.Pubrec do
  alias Creep.Packet.Pubrec

  @type t() :: %Pubrec{
          packet_id: integer()
        }

  defstruct [
    :packet_id
  ]

  defimpl Creep.Packet.Encode, for: Pubrec do
    @type_pubrec 0x05

    def encode(%Pubrec{packet_id: packet_id}) do
      <<@type_pubrec::4, 0::4, 2::8, packet_id::16>>
    end
  end

  defimpl Creep.Packet.Decode, for: Pubrec do
    @type_pubrec 0x05

    def decode(%Pubrec{} = packet, <<
          @type_pubrec::4,
          _reserved::4,
          2::8,
          packet_id::16
        >>) do
      %{packet | packet_id: packet_id}
    end
  end
end
