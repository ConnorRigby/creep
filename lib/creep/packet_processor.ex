defmodule Creep.PacketProcessor do
  @moduledoc "behaviour responsible for implementing the mqtt state machine"

  alias Creep.Packet.{
    Connect,
    Connack,
    Publish,
    Puback,
    Pubrec,
    Pubrel,
    Pubcomp,
    Subscribe,
    Suback,
    Unsubscribe,
    Unsuback,
    Pingreq,
    Pingresp,
    Disconnect
  }

  @type session_id() :: term()

  @doc "supervisor spec"
  @callback child_spec([Creep.broker_opt()]) :: Supervisor.child_spec()

  @doc "Process a connection"
  @callback connect(Creep.broker_id(), Connect.t()) :: {Connack.t(), session_id()}

  @doc "Process a publish packet"
  @callback publish(Creep.broker_id(), session_id(), Publish.t()) ::
              nil | Puback.t() | Pubrec.t()

  @doc "Process a pubrel packet"
  @callback pubrel(Creep.broker_id(), session_id(), Pubrel.t()) :: Pubcomp.t()

  @doc "Process a subcrcibe packet"
  @callback subscribe(Creep.broker_id(), session_id(), Subscribe.t()) :: Suback.t()

  @doc "Procses an unsubscribe packet"
  @callback unsubscribe(Creep.broker_id(), session_id(), Unsubscribe.t()) :: Unsuback.t()

  @doc "Process a pingreq packet"
  @callback pingreq(Creep.broker_id(), session_id(), Pingreq.t()) :: Pingresp.t()

  @doc "Process a disconnect packet"
  @callback disconnect(Creep.broker_id(), session_id(), Disconnect.t()) :: any()
end
