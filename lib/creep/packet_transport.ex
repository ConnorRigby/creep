defmodule Creep.PacketTransport do
  @moduledoc """
  Behaviour responsible to implementing the transport of packets. This should be
  an OTP process that allows for only the transport of validated packets. This
  process should only be concerned with transporting and validating valid
  MQTT packets. It should not implement the MQTT state machine, but dispatch
  validated packets to the Creep.PacketProcessor implementation passed in to
  the initial options.
  """

  @doc "Supervisor spec"
  @callback child_spec([Creep.broker_opt()]) :: Supervisor.child_spec()
end
