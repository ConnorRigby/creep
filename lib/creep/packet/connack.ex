defmodule Creep.Packet.Connack do
  alias Creep.Packet.Connack

  @type return_code ::
          :accepted
          | :unacceptable_protocol
          | :identifier_rejected
          | :server_unavailable
          | :bad_username_or_password

  @type t() :: %Connack{
          session_present: boolean(),
          return_code: return_code()
        }

  defstruct session_present: false,
            return_code: :accepted

  defimpl Creep.Packet.Encode, for: Connack do
    import Creep.Packet.Util
    @type_connack 0x02
    def encode(%Connack{session_present: sp, return_code: rc}) do
      <<
        @type_connack::4,
        0::4,
        0x02::8,
        0::1,
        0::1,
        0::1,
        0::1,
        0::1,
        0::1,
        0::1,
        bool(sp)::1,
        return_code(rc)::8
      >>
    end

    def return_code(:accepted), do: 0x00
    def return_code(:unacceptable_protocol), do: 0x01
    def return_code(:identifier_rejected), do: 0x02
    def return_code(:server_unavailable), do: 0x03
    def return_code(:bad_username_or_password), do: 0x04
  end
end
