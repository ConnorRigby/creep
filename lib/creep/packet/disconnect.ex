defmodule Creep.Packet.Disconnect do
  alias Creep.Packet.Disconnect

  @type t() :: %Disconnect{}
  defstruct []

  defimpl Creep.Packet.Encode, for: Disconnect do
    @type_disconnect 0xE
    def encode(%Disconnect{} = _disconnect) do
      <<@type_disconnect::4, 0::4, 0::8>>
    end
  end

  defimpl Creep.Packet.Decode, for: Disconnect do
    def decode(_, <<_type_disconnect::4, _reserved::4, _::binary>>) do
      %Disconnect{}
    end
  end
end
