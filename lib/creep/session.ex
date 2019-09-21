defmodule Creep.Session do
  defstruct [
    :client_id,
    :last_will_retain,
    :last_will_qos,
    :last_will_topic,
    :last_will_message,
    :ref,
    :pid,
    topic_filters: []
  ]
end
