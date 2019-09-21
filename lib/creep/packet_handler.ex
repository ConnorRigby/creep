defmodule Creep.PacketHandler do
  @moduledoc "behaviour responsible for implementing the mqtt state machine"

  alias Packet.{
    Connect,
    Connack,
    Publish,
    Puback,
    Pubrel,
    Pubcomp,
    Subscribe,
    Suback,
    Unsubscribe,
    Unsuback,
    Pingreq,
    Disconnect
  }

  @doc "Process a connection"
  @callback connect(Creep.broker_id(), Connect.t()) :: Connack.t()

  @doc "Process a publish packet"
  @callback publish(Creep.broker_id(), session_id :: term(), Publish.t()) ::
              Puback.t() | Pubreq.t()

  @doc "Process a pubrel packet"
  @callback pubrel(Creep.broker_id(), session_id :: term(), Pubrel.t()) :: Pubcomp.t()

  @doc "Process a subcrcibe packet"
  @callback subscribe(Creep.broker_id(), session_id :: term(), Subscribe.t()) :: Suback.t()

  @doc "Procses an unsubscribe package"
  @callback unsubscribe(Creep.broker_id(), session_id :: term(), Unsubscribe.t()) :: Unsuback.t()

  @doc "Process a pingreq packet"
  @callback pingreq(Creep.broker_id(), session_id :: term(), Pingreq.t()) :: Pinresp.t()

  @doc "Process a disconnect packet"
  @callback disconnect(Creep.broker_id(), session_id :: term(), Disconnect.t()) :: any()
end
