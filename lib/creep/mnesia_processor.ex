defmodule Creep.MnesiaProcessor do
  @behaviour Creep.PacketProcessor

  require Logger

  alias Creep.{
    Session,
    TopicFilter
  }

  alias Creep.Packet.{
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

  @type session_record :: tuple()

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

    session_attrs = [
      :client_id,
      :broker_id,
      :ref,
      :pid,
      :last_will_retain,
      :last_will_qos,
      :last_will_topic,
      :last_will_message,
      :topic_filters,
      :publishes
    ]

    with {:atomic, :ok} <-
           :mnesia.create_table(Session, attributes: session_attrs, index: [:broker_id, :pid]) do
      :ignore
    else
      {:aborted, {:already_exists, Session}} -> :ignore
      {:aborted, reason} -> {:error, reason}
    end
  end

  def get_session_by_client_id(broker_id, client_id) do
    match = fn ->
      match = %Session{
        last_will_retain: :_,
        last_will_qos: :_,
        last_will_topic: :_,
        last_will_message: :_,
        ref: :_,
        pid: :_,
        topic_filters: :_,
        publishes: :_,
        client_id: client_id,
        broker_id: broker_id
      }

      :mnesia.match_object(to_record(match))
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

  @spec new_session(Session.t() | session_record()) ::
          {:ok, Session.t()} | {:error, reason :: term()}
  def new_session(%Session{} = session) do
    session
    |> to_record()
    |> new_session()
  end

  def new_session(session_record) when is_tuple(session_record) do
    write = fn -> :mnesia.write(session_record) end

    case :mnesia.transaction(write) do
      {:atomic, :ok} -> {:ok, to_elixir(session_record)}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec update_session(Session.t() | session_record(), map()) ::
          {:ok, Session.t()} | {:error, reason :: term()}
  def update_session(%Session{} = session, params) do
    session
    |> struct!(params)
    |> to_record()
    |> new_session()
  end

  def update_session(session_record, params) when is_tuple(session_record) do
    session_record
    |> to_elixir()
    |> update_session(params)
  end

  def get_sessions_matching_topic(broker_id, topic) do
    match = fn ->
      match = %Session{
        last_will_retain: :_,
        last_will_qos: :_,
        last_will_topic: :_,
        last_will_message: :_,
        ref: :_,
        pid: :_,
        topic_filters: :_,
        publishes: :_,
        client_id: :_,
        broker_id: broker_id
      }

      :mnesia.match_object(to_record(match))
    end

    case :mnesia.transaction(match) do
      {:atomic, data} when is_list(data) ->
        filtered =
          data
          |> Enum.map(&to_elixir/1)
          |> Enum.filter(fn %{topic_filters: topic_filters} ->
            topic_filters = Enum.map(topic_filters, fn {topic_filter, _qos} -> topic_filter end)
            TopicFilter.matches_any_filters?(topic_filters, topic)
          end)

        {:ok, filtered}

      {:aborted, reason} ->
        {:error, reason}
    end
  end

  @spec add_publish_to_session(Session.t(), Publish.t()) :: Session.t()
  def add_publish_to_session(%Session{} = session, %Publish{} = publish) do
    case update_session(session, %{publishes: [publish | session.publishes]}) do
      {:ok, session} -> session
      _ -> session
    end
  end

  @spec clear_publish_from_session(Session.t(), Puback.t() | Pubrel.t()) :: Session.t()
  def clear_publish_from_session(%Session{} = session, %_type{packet_id: packet_id}) do
    publishes =
      Enum.reject(session.publishes, fn
        %{packet_id: ^packet_id} -> true
        _ -> false
      end)

    case update_session(session, %{publishes: publishes}) do
      {:ok, session} -> session
      _ -> session
    end
  end

  @impl Creep.PacketProcessor
  def connect(broker_id, connect) do
    session_pid = self()

    case get_session_by_client_id(broker_id, connect.client_id) do
      {:ok, %Session{} = session} ->
        params = %{
          broker_id: broker_id,
          ref: make_ref(),
          pid: session_pid,
          client_id: connect.client_id,
          last_will_retain: connect.last_will_retain,
          last_will_qos: connect.last_will_qos,
          last_will_topic: connect.last_will_topic,
          last_will_message: connect.last_will_message
        }

        if session.pid && Process.alive?(session.pid) do
          Logger.info("Disconnecting #{session.client_id}")
          send(session.pid, %Disconnect{})
        end

        {:ok, new_sesion} = update_session(session, params)
        {%Connack{session_present: true, return_code: :accepted}, new_sesion}

      nil ->
        session = %Session{
          broker_id: broker_id,
          ref: make_ref(),
          pid: session_pid,
          client_id: connect.client_id,
          last_will_retain: connect.last_will_retain,
          last_will_qos: connect.last_will_qos,
          last_will_topic: connect.last_will_topic,
          last_will_message: connect.last_will_message
        }

        {:ok, session} = new_session(session)
        {%Connack{session_present: false, return_code: :accepted}, session}
    end
  end

  @impl Creep.PacketProcessor
  def publish(broker_id, session, %Publish{qos: 0} = publish) do
    {:ok, sessions} = get_sessions_matching_topic(broker_id, publish.topic)

    for %{pid: pid} when is_pid(pid) <- sessions do
      Process.alive?(pid) && send(pid, publish)
    end

    {nil, session}
  end

  def publish(broker_id, session, %Publish{qos: 1} = publish) do
    {:ok, sessions} = get_sessions_matching_topic(broker_id, publish.topic)

    for %{pid: pid} when is_pid(pid) <- sessions do
      Process.alive?(pid) && send(pid, publish)
    end

    puback = %Puback{
      packet_id: publish.packet_id
    }

    {puback, add_publish_to_session(session, publish)}
  end

  def publish(broker_id, session, %Publish{qos: 2} = publish) do
    {:ok, sessions} = get_sessions_matching_topic(broker_id, publish.topic)

    for %{pid: pid} when is_pid(pid) <- sessions do
      Process.alive?(pid) && send(pid, publish)
    end

    pubrec = %Pubrec{
      packet_id: publish.packet_id
    }

    {pubrec, add_publish_to_session(session, publish)}
  end

  @impl Creep.PacketProcessor
  def puback(_broker_id, session, %Puback{} = puback) do
    {nil, clear_publish_from_session(session, puback)}
  end

  @impl Creep.PacketProcessor
  def pubrel(_broker_id, session, %Pubrel{} = pubrel) do
    pubcomp = %Pubcomp{
      packet_id: pubrel.packet_id
    }

    {pubcomp, clear_publish_from_session(session, pubrel)}
  end

  @impl Creep.PacketProcessor
  def subscribe(_broker_id, session, %Subscribe{
        packet_id: packet_id,
        topic_filters: topic_filters
      }) do
    new_topic_filters = Enum.uniq(session.topic_filters ++ topic_filters)
    {:ok, new_session} = update_session(session, %{topic_filters: new_topic_filters})

    suback = %Suback{
      packet_id: packet_id,
      responses: Enum.map(topic_filters, fn {_, qos} -> qos end)
    }

    {suback, new_session}
  end

  @impl Creep.PacketProcessor
  def unsubscribe(_broker_id, session, %Unsubscribe{} = unsubscribe) do
    new_topic_filters =
      Enum.reject(session.topic_filters, fn {session_filter, _qos} ->
        TopicFilter.matches_any_filters?(unsubscribe.topic_filters, session_filter)
      end)

    {:ok, new_session} = update_session(session, %{topic_filters: new_topic_filters})
    unsuback = %Unsuback{packet_id: unsubscribe.packet_id}
    {unsuback, new_session}
  end

  @impl Creep.PacketProcessor
  def pingreq(_broker_id, session, %Pingreq{}) do
    {%Pingresp{}, session}
  end

  @impl Creep.PacketProcessor
  def disconnect(broker_id, %Session{pid: pid} = session, %{reason: reason}) do
    case get_session_by_client_id(broker_id, session.client_id) do
      {:ok, %{pid: ^pid}} when is_pid(pid) and reason == :normal ->
        Logger.info("disconnecting session")
        {:ok, _new_session} = update_session(session, %{ref: nil, pid: nil, topic_filters: []})

      {:ok, %{pid: ^pid}} when is_pid(pid) and reason == :normal ->
        Logger.info("disconnecting session due to crash")
        {:ok, _new_session} = update_session(session, %{ref: nil, pid: nil})

      {:ok, _stored} ->
        Logger.info("session already disconnected")
        {:ok, session}

      nil ->
        Logger.warn("disconnected unknown session?")
        {:ok, session}
    end
  end

  def to_elixir(
        {Session, client_id, broker_id, ref, pid, last_will_retain, last_will_qos,
         last_will_topic, last_will_message, topic_filters, publishes}
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
      topic_filters: topic_filters,
      publishes: publishes
    }
  end

  def to_elixir(%Session{} = session), do: session

  def to_record(%Session{
        client_id: client_id,
        broker_id: broker_id,
        ref: ref,
        pid: pid,
        last_will_retain: last_will_retain,
        last_will_qos: last_will_qos,
        last_will_topic: last_will_topic,
        last_will_message: last_will_message,
        topic_filters: topic_filters,
        publishes: publishes
      }) do
    {Session, client_id, broker_id, ref, pid, last_will_retain, last_will_qos, last_will_topic,
     last_will_message, topic_filters, publishes}
  end

  def to_record(session_record) when is_tuple(session_record), do: session_record
end
