defmodule Creep.MnesiaProcessor do
  @behaviour Creep.PacketProcessor

  alias Creep.Session

  alias Creep.Packet.{
    Connack
  }

  @impl Creep.PacketProcessor
  def child_spec(opts) do
    broker_id = Keyword.fetch!(opts, :broker_id)

    %{
      id: {__MODULE__, broker_id},
      start: {__MODULE__, :start_link, [opts]}
    }
  end

  def start_link(_opts) do
    _ = :mnesia.create_schema([node()])
    :ok = :mnesia.start()

    attrs = [
      :client_id,
      :broker_id,
      :ref,
      :pid,
      :last_will_retain,
      :last_will_qos,
      :last_will_topic,
      :last_will_message,
      :topic_filters
    ]

    case :mnesia.create_table(Session, attributes: attrs, index: [:broker_id, :pid]) do
      {:atomic, :ok} -> :ignore
      {:aborted, {:already_exists, Session}} -> :ignore
      {:aborted, reason} -> {:aborted, reason}
    end
  end

  def get_session_by_client_id(broker_id, client_id) do
    match = fn ->
      :mnesia.match_object(to_record(%Session{client_id: :_, broker_id: broker_id}))
    end

    case :mnesia.transaction(match) do
      {:atomic, [session]} ->
        {:ok, to_elixir(session)}

      {:atomic, [_session | _] = sessions} ->
        {:error, "expected at most one match but got: #{Enum.count(sessions)}"}

      {:atomic, []} ->
        nil

      {:error, reason} ->
        {:error, reason}
    end
  end

  def new_session(%Session{} = session) do
    session
    |> to_record()
    |> new_session()
  end

  def new_session(session_record) when is_tuple(session_record) do
    write = fn -> :mnesia.write(session_record) end

    case :mnesia.transaction(write) do
      {:atomic, :ok} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  def update_session(%Session{} = session, params) do
    new_session(to_record(struct!(session, params)))
  end

  @impl Creep.PacketProcessor
  def connect(broker_id, connect) do
    session_pid = self()

    case get_session_by_client_id(broker_id, connect.client_id) do
      {:ok, %Session{} = session} ->
        params = %{
          ref: make_ref(),
          pid: session_pid,
          client_id: connect.client_id,
          last_will_retain: connect.last_will_retain,
          last_will_qos: connect.last_will_qos,
          last_will_topic: connect.last_will_topic,
          last_will_message: connect.last_will_message
        }

        :ok = update_session(session, params)
        {%Connack{session_present: true, return_code: :accepted}, session}

      nil ->
        session = %Session{
          ref: make_ref(),
          pid: session_pid,
          client_id: connect.client_id,
          last_will_retain: connect.last_will_retain,
          last_will_qos: connect.last_will_qos,
          last_will_topic: connect.last_will_topic,
          last_will_message: connect.last_will_message
        }

        :ok = new_session(session)
        {%Connack{session_present: false, return_code: :accepted}, session}
    end
  end

  @impl Creep.PacketProcessor
  def publish(broker_id, session_id, publish) do
  end

  @impl Creep.PacketProcessor
  def pubrel(broker_id, session_id, pubrel) do
  end

  @impl Creep.PacketProcessor
  def subscribe(broker_id, session_id, subscribe) do
  end

  @impl Creep.PacketProcessor
  def unsubscribe(broker_id, session_id, unsubscribe) do
  end

  @impl Creep.PacketProcessor
  def pingreq(broker_id, session_id, pingreq) do
  end

  @impl Creep.PacketProcessor
  def disconnect(broker_id, session, disconnect) do
    update_session(session, %{ref: nil, pid: nil, topic_filters: []})
  end

  defp to_elixir(
         {Session, client_id, broker_id, ref, pid, last_will_retain, last_will_qos,
          last_will_topic, last_will_message, topic_filters}
       ) do
    %Session{
      client_id: client_id,
      broker_id: broker_id,
      ref: ref,
      pid: pid,
      last_will_retain: last_will_retain,
      last_will_qos: last_will_qos,
      last_will_topic: last_will_topic,
      last_will_message: last_will_message,
      topic_filters: topic_filters
    }
  end

  defp to_elixir(%Session{} = session) do
    session
  end

  defp to_record(%Session{
         client_id: client_id,
         broker_id: broker_id,
         ref: ref,
         pid: pid,
         last_will_retain: last_will_retain,
         last_will_qos: last_will_qos,
         last_will_topic: last_will_topic,
         last_will_message: last_will_message,
         topic_filters: topic_filters
       }) do
    {Session, client_id, broker_id, ref, pid, last_will_retain, last_will_qos, last_will_topic,
     last_will_message, topic_filters}
  end

  defp to_record(session_record) when is_tuple(session_record) do
    session_record
  end
end
