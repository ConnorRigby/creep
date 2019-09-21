defmodule Creep.PacketTest do
  alias Creep.Packet
  alias Packet.Connect

  use ExUnit.Case
  doctest Packet

  describe "connect" do
    test "usernam, password" do
      {:ok, decoded} =
        Packet.decode(
          <<16, 62, 0, 4, 77, 81, 84, 84, 4, 194, 0, 60, 0, 32, 49, 100, 55, 55, 52, 50, 55, 97,
            57, 98, 101, 57, 52, 51, 97, 98, 97, 102, 99, 99, 54, 98, 50, 56, 48, 100, 56, 99, 48,
            48, 48, 102, 0, 6, 99, 111, 110, 110, 111, 114, 0, 8, 112, 97, 115, 115, 119, 111,
            114, 100>>
        )

      assert decoded == %Connect{
               protocol_name: "MQTT",
               protocol_version: 4,
               clean_session: true,
               keep_alive: 60,
               client_identifier: "1d77427a9be943abafcc6b280d8c000f",
               username: "connor",
               password: "password",
               last_will_retain: false,
               last_will_qos: 0,
               last_will_topic: nil,
               last_will_message: nil,
               error: false
             }
    end

    test "username, password, lwt qos=0" do
      {:ok, decoded} =
        Packet.decode(
          <<16, 98, 0, 4, 77, 81, 84, 84, 4, 230, 0, 60, 0, 32, 49, 100, 55, 55, 52, 50, 55, 97,
            57, 98, 101, 57, 52, 51, 97, 98, 97, 102, 99, 99, 54, 98, 50, 56, 48, 100, 56, 99, 48,
            48, 48, 102, 0, 15, 108, 97, 115, 116, 32, 119, 105, 108, 108, 32, 116, 111, 112, 105,
            99, 0, 17, 108, 97, 115, 116, 32, 119, 105, 108, 108, 32, 99, 111, 110, 116, 101, 110,
            116, 0, 6, 99, 111, 110, 110, 111, 114, 0, 8, 112, 97, 115, 115, 119, 111, 114, 100>>
        )

      assert decoded == %Connect{
               protocol_name: "MQTT",
               protocol_version: 4,
               clean_session: true,
               keep_alive: 60,
               client_identifier: "1d77427a9be943abafcc6b280d8c000f",
               username: "connor",
               password: "password",
               last_will_retain: true,
               last_will_qos: 0,
               last_will_topic: "last will topic",
               last_will_message: "last will content",
               error: false
             }
    end
  end
end
