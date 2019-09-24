defmodule Creep.PacketProcessor do
  @moduledoc "behaviour responsible for implementing the mqtt state machine"

  alias Creep.Session

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

  @doc "supervisor spec"
  @callback child_spec([Creep.broker_opt()]) :: Supervisor.child_spec()

  @doc "Process a connection"
  @callback connect(Creep.broker_id(), Connect.t()) :: {Connack.t(), Session.t()}

  @doc "Process a publish packet"
  @callback publish(Creep.broker_id(), Session.t(), Publish.t()) ::
              {nil | Puback.t() | Pubrec.t(), Session.t()}

  @doc "Process a pubrel packet"
  @callback pubrel(Creep.broker_id(), Session.t(), Pubrel.t()) :: {Pubcomp.t(), Session.t()}

  @doc "Process a subcrcibe packet"
  @callback subscribe(Creep.broker_id(), Session.t(), Subscribe.t()) :: {Suback.t(), Session.t()}

  @doc "Procses an unsubscribe packet"
  @callback unsubscribe(Creep.broker_id(), Session.t(), Unsubscribe.t()) ::
              {Unsuback.t(), Session.t()}

  @doc "Process a pingreq packet"
  @callback pingreq(Creep.broker_id(), Session.t(), Pingreq.t()) :: {Pingresp.t(), Session.t()}

  @doc "Process a disconnect packet"
  @callback disconnect(Creep.broker_id(), Session.t(), Disconnect.t()) :: any()
end
