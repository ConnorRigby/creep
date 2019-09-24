defmodule Creep.Session do
  @moduledoc """
  Represents a single connection. A connection
  may or may not be currently active
  """

  alias Creep.Session

  @type t() :: %Session{
          client_id: Creep.Packet.Connect.client_id(),
          last_will_retain: boolean(),
          last_will_qos: 0..2,
          last_will_topic: String.t() | nil,
          last_will_message: String.t() | nil,
          ref: reference(),
          pid: pid(),
          topic_filters: [{String.t(), 0..2}],
          broker_id: Creep.broker_id()
        }

  defstruct [
    :client_id,
    :last_will_retain,
    :last_will_qos,
    :last_will_topic,
    :last_will_message,
    :ref,
    :pid,
    :broker_id,
    topic_filters: []
  ]
end
