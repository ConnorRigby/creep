defmodule Creep.Packet.Connect do
  alias Creep.Packet.Connect

  @type client_id :: String.t()

  @type t :: %Connect{
          protocol_name: String.t(),
          protocol_version: integer(),
          clean_session: boolean(),
          keep_alive: integer(),
          client_id: client_id(),
          username: nil | String.t(),
          password: nil | String.t(),
          last_will_retain: boolean(),
          last_will_qos: 0..2,
          last_will_topic: nil | String.t(),
          last_will_message: nil | String.t(),
          error: false | term()
        }

  defstruct [
    :protocol_name,
    :protocol_version,
    :clean_session,
    :keep_alive,
    :client_id,
    :username,
    :password,
    :last_will_retain,
    :last_will_qos,
    :last_will_topic,
    :last_will_message,
    error: false
  ]

  defimpl Creep.Packet.Decode, for: Connect do
    import Creep.Packet.Util

    def decode(packet, <<
          _type_connect::integer-4,
          _flags::bits-4,
          payload_size::integer-8,
          payload::binary-size(payload_size)
        >>) do
      <<
        # Protocol details
        protocol_name_size::integer-16,
        protocol_name::binary-size(protocol_name_size),
        protocol_version::integer-8,

        # connect flags
        connect_flags::8-bits,

        # keep alive
        keep_alive::integer-16,

        # The Client Identifier (ClientId) MUST be present and MUST be the first field in the CONNECT packet payload [MQTT-3.1.3-3].
        client_id_size::integer-16,
        client_id::binary-size(client_id_size),
        payload::binary
      >> = payload

      packet = %Connect{
        packet
        | client_id: client_id,
          keep_alive: keep_alive,
          protocol_name: protocol_name,
          protocol_version: protocol_version
      }

      {_, <<>>, packet} =
        {connect_flags, payload, packet}
        |> decode_connect_will_flags()
        |> decode_connect_username()
        |> decode_connect_password()

      packet
    end

    # <<
    #   username::1,
    #   password::1,
    #   will_retain::1,
    #   will_qos::bits-size(2),
    #   will_flag::1,
    #   clean_session::1,
    #   _reserved::1
    # >>

    # will_flag=1
    defp decode_connect_will_flags(
           {<<_::1, _::1, will_retain::1, will_qos::2, 1::1, clean_session::1, _reserved::1>> =
              flags,
            <<
              will_topic_size::integer-16,
              will_topic::binary-size(will_topic_size),
              will_message_size::integer-16,
              will_message::binary-size(will_message_size),
              rest::binary
            >>, packet}
         ) do
      packet = %{
        packet
        | last_will_topic: will_topic,
          last_will_message: will_message,
          last_will_retain: bool(will_retain),
          last_will_qos: will_qos,
          clean_session: bool(clean_session)
      }

      {flags, rest, packet}
    end

    # Error if will_flag=1 but no will is in the payload
    # If the Will Flag is set to 1, the Will QoS and Will Retain fields in the Connect Flags will be used by the Server, and the Will Topic and Will Message fields MUST be present in the payload [MQTT-3.1.2-9].
    # If the Will Flag is set to 0 the Will QoS and Will Retain fields in the Connect Flags MUST be set to zero and the Will Topic and Will Message fields MUST NOT be present in the payload
    defp decode_connect_will_flags(
           {<<_::1, _::1, will_retain::1, will_qos::2, 1::1, clean_session::1, _reserved::1>> =
              flags, payload, packet}
         ) do
      packet = %{
        packet
        | last_will_qos: will_qos,
          last_will_retain: bool(will_retain),
          clean_session: bool(clean_session),
          error: "will_flag was set, but no last will message or topic were supplied"
      }

      {flags, payload, packet}
    end

    defp decode_connect_will_flags(
           {<<_::1, _::1, _::1, will_qos::2, 0::1, clean_session::1, _reserved::1>> = flags,
            payload, packet}
         ) do
      {flags, payload,
       %{
         packet
         | last_will_qos: will_qos,
           last_will_retain: false,
           clean_session: bool(clean_session)
       }}
    end

    # username=1
    defp decode_connect_username(
           {<<1::1, _::7>> = flags,
            <<
              username_size::integer-16,
              username::binary-size(username_size),
              rest::binary
            >>, packet}
         ) do
      {flags, rest, %{packet | username: username}}
    end

    defp decode_connect_username({flags, payload, packet}) do
      {flags, payload, packet}
    end

    # password=1
    defp decode_connect_password(
           {<<_::1, 1::1, _::6>> = flags,
            <<
              password_size::integer-16,
              password::binary-size(password_size)
            >>, packet}
         ) do
      {flags, <<>>, %{packet | password: password}}
    end

    defp decode_connect_password({flags, payload, packet}) do
      {flags, payload, packet}
    end
  end
end
