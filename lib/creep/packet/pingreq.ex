defmodule Creep.Packet.Pingreq do
  alias Creep.Packet.Pingreq
  defstruct []

  defimpl Creep.Packet.Encode, for: Pingreq do
    @type_pingreq 0xC
    def encode(%Pingreq{} = _pingreq) do
      <<@type_pingreq::4, 0::4, 0::8>>
    end
  end

  defimpl Creep.Packet.Decode, for: Pingreq do
    def decode(_, <<_type_pingreq::4, _reserved::4, _::binary>>) do
      %Pingreq{}
    end
  end
end
