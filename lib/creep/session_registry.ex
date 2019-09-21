defmodule Creep.SessionRegistry do
  @moduledoc """
  Tracks all connections to a broker
  """

  @behaviour Creep.PacketHandler

  alias Creep.Packet.{
    Connect,
    Connack,
    Publish,
    Subscribe,
    Suback,
    Pingreq,
    Pingresp
  }

  alias Creep.Session

  use GenServer
  alias __MODULE__, as: State
  defstruct [:sessions]

  @impl Creep.PacketHandler
  def connect(broker_id, connect) do
    name = Module.concat(__MODULE__, broker_id)
    GenServer.call(name, {:connect, connect})
  end

  @impl Creep.PacketHandler
  def publish(broker_id, session_id, publish) do
    name = Module.concat(__MODULE__, broker_id)
    GenServer.call(name, {:publish, session_id, publish})
  end

  @impl Creep.PacketHandler
  def pubrel(broker_id, session_id, pubrel) do
    name = Module.concat(__MODULE__, broker_id)
    GenServer.call(name, {:pubrel, session_id, pubrel})
  end

  @impl Creep.PacketHandler
  def subscribe(broker_id, session_id, subscribe) do
    name = Module.concat(__MODULE__, broker_id)
    GenServer.call(name, {:subscribe, session_id, subscribe})
  end

  @impl Creep.PacketHandler
  def unsubscribe(broker_id, session_id, unsubscribe) do
    name = Module.concat(__MODULE__, broker_id)
    GenServer.call(name, {:unsubscribe, session_id, unsubscribe})
  end

  @impl Creep.PacketHandler
  def pingreq(broker_id, session_id, pingreq) do
    name = Module.concat(__MODULE__, broker_id)
    GenServer.call(name, {:pingreq, session_id, pingreq})
  end

  @impl Creep.PacketHandler
  def disconnect(broker_id, session_id, disconnect) do
    name = Module.concat(__MODULE__, broker_id)
    GenServer.call(name, {:disconnect, session_id, disconnect})
  end

  @doc false
  @spec start_link(Creep.broker_opts()) :: GenServer.on_start()
  def start_link(args) do
    broker_id = Keyword.fetch!(args, :broker_id)
    name = Module.concat(__MODULE__, broker_id)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @impl GenServer
  def init(_args) do
    {:ok, %State{sessions: %{}}}
  end

  @impl GenServer

  # he Server MUST respond to the CONNECT Packet with a CONNACK return code 0x01 (unacceptable protocol level) and then disconnect the Client if the Protocol Level is not supported by the Server [MQTT-3.1.2-2]
  def handle_call({:connect, %Connect{protocol_version: v}}, _from, state)
      when v != 4 do
    {:reply, %Connack{return_code: :unacceptable_protocol}, state}
  end

  def handle_call({:connect, %Connect{} = connect}, {pid, _tag}, state) do
    {state, reply} = add_session_to_state(state, connect, pid)
    {:reply, reply, state}
  end

  def handle_call({:publish, _session_id, %Publish{qos: 0} = publish}, _from, state) do
    do_publish(state, publish)
    {:reply, nil, state}
  end

  def handle_call({:pubrel, _session_id, _pubrel}, _from, _state) do
  end

  def handle_call({:subscribe, session_id, subscribe}, _from, state) do
    {state, reply} = add_subscription_to_state(state, subscribe, session_id)
    {:reply, reply, state}
  end

  def handle_call({:unsubscribe, _session_id, _unsubscribe}, _from, _state) do
  end

  def handle_call({:pingreq, _session_id, %Pingreq{}}, _from, state) do
    {:reply, %Pingresp{}, state}
  end

  defp add_session_to_state(state, %Connect{clean_session: true} = connect, pid) do
    session = new_session(connect, pid)
    reply = {%Connack{session_present: false, return_code: :accepted}, session.ref}
    {%{state | sessions: Map.put(state.sessions, session.ref, session)}, reply}
  end

  defp add_session_to_state(
         state,
         %Connect{client_id: client_id, clean_session: false} = connect,
         pid
       ) do
    if session = state.sessions[client_id] do
      session = update_session(session, connect, pid)
      reply = {%Connack{session_present: true, return_code: :accepted}, session.ref}
      {%{state | sessions: Map.put(state.sessions, session.ref, session)}, reply}
    else
      session = new_session(connect, pid)
      reply = {%Connack{session_present: false, return_code: :accepted}, session.ref}
      {%{state | sessions: Map.put(state.sessions, session.ref, session)}, reply}
    end
  end

  defp new_session(%Connect{} = connect, pid) do
    %Session{
      ref: make_ref(),
      pid: pid,
      client_id: connect.client_id,
      last_will_retain: connect.last_will_retain,
      last_will_qos: connect.last_will_qos,
      last_will_topic: connect.last_will_topic,
      last_will_message: connect.last_will_message
    }
  end

  defp update_session(session, connect, pid) do
    %Session{
      session
      | pid: pid,
        last_will_retain: connect.last_will_retain,
        last_will_qos: connect.last_will_qos,
        last_will_topic: connect.last_will_topic,
        last_will_message: connect.last_will_message
    }
  end

  defp add_subscription_to_state(
         state,
         %Subscribe{packet_id: packet_id, topic_filters: topic_filters},
         session_id
       ) do
    session = Map.fetch!(state.sessions, session_id)
    new_session = %Session{session | topic_filters: session.topic_filters ++ topic_filters}

    reply = %Suback{
      packet_id: packet_id,
      responses: Enum.map(topic_filters, fn {_, qos} -> qos end)
    }

    {%{state | sessions: Map.put(state.sessions, session_id, new_session)}, reply}
  end

  defp do_publish(state, publish) do
    Enum.filter(state.sessions, fn {_, %Session{topic_filters: topic_filters}} ->
      matches_any_filters?(topic_filters, publish)
    end)
    |> Enum.each(fn {_, %Session{ref: ref, pid: pid}} ->
      IO.puts("publishing packet to #{inspect(ref)}")
      send(pid, publish)
    end)

    :ok
  end

  defp matches_any_filters?([{filter, _qos} | rest], %Publish{topic: topic}) do
    if matches_filter?(filter, topic) do
      true
    else
      matches_any_filters?(rest, topic)
    end
  end

  defp matches_any_filters?([], _), do: false

  defp matches_filter?(filter, topic) when is_binary(filter) do
    matches_filter?(Path.split(filter), Path.split(topic))
  end

  defp matches_filter?(["#" | _], _), do: true

  defp matches_filter?([section | filter_rest], [section | topic_rest]) do
    matches_filter?(filter_rest, topic_rest)
  end

  defp matches_filter?([], []), do: true
end