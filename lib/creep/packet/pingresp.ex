defmodule Creep.Packet.Pingresp do
  alias Creep.Packet.Pingresp

  @type t() :: %Pingresp{}

  defstruct []

  defimpl Creep.Packet.Encode, for: Pingresp do
    @type_pingresp 0xD
    def encode(%Pingresp{} = _pingresp) do
      <<@type_pingresp::4, 0::4, 0::8>>
    end
  end

  defimpl Creep.Packet.Decode, for: Pingresp do
    def decode(_, <<_type_pingresp::4, _reserved::4, _::binary>>) do
      %Pingresp{}
    end
  end
end
