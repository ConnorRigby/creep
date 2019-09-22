defmodule Creep.TortoiseIntegrationTest do
  use ExUnit.Case, async: true

  defmodule IntegrationHandler do
    @behaviour Tortoise.Handler

    @impl Tortoise.Handler
    def init(args) do
      test_pid = Keyword.fetch!(args, :pid)
      {:ok, %{test_pid: test_pid}}
    end

    @impl Tortoise.Handler
    def connection(status, state) do
      send(state.test_pid, {:connection, status})
      {:ok, state}
    end

    @impl Tortoise.Handler
    def handle_message(topic_levels, payload, state) do
      send(state.test_pid, {:message, topic_levels, payload})
      {:ok, state}
    end

    @impl Tortoise.Handler
    def subscription(status, topic_filter, state) do
      send(state.test_pid, {:subscription, status, topic_filter})
      {:ok, state}
    end

    @impl Tortoise.Handler
    def terminate(reason, state) do
      send(state.test_pid, {:terminate, reason})
      {:ok, state}
    end
  end

  setup do
    broker_id = :rand.uniform() |> to_string()
    client_id = :rand.uniform() |> to_string()
    port = Enum.random(60000..60123)

    {:ok, pid} =
      Creep.start_link(
        broker_id: broker_id,
        packet_processor: Creep.InMemProcessor,
        transports: [
          {Creep.RanchTransport, [port: port]}
        ]
      )

    {:ok, %{broker: pid, broker_id: broker_id, client_id: client_id, port: port}}
  end

  describe "connection" do
    test "connect", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: []
        )

      assert_receive {:connection, :up}
    end

    test "connect with subscriptions", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: [{"a/b", 0}, {"c/d", 1}, {"e/f", 2}]
        )

      assert_receive {:connection, :up}
      assert_receive {:subscription, :up, "a/b"}
      assert_receive {:subscription, :up, "c/d"}
      assert_receive {:subscription, :up, "e/f"}
    end

    test "connect w/ username password", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          username: "connor",
          password: "password",
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: []
        )

      assert_receive {:connection, :up}
    end
  end

  describe "publish/subscribe" do
    test "subscribe", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: []
        )

      :ok = Tortoise.Connection.subscribe_sync(client_id, [{"a/b", 0}])
      assert_receive {:subscription, :up, "a/b"}

      :ok = Tortoise.publish(client_id, "a/b", "hello, world")
      assert_receive {:message, ["a", "b"], "hello, world"}
    end

    test "wildcard subscribe", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: []
        )

      :ok = Tortoise.Connection.subscribe_sync(client_id, [{"a/#", 0}])
      assert_receive {:subscription, :up, "a/#"}

      :ok = Tortoise.publish(client_id, "a/b", "hello, world")
      assert_receive {:message, ["a", "b"], "hello, world"}
    end

    test "unsubscribe", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: []
        )

      assert_receive {:connection, :up}

      :ok = Tortoise.Connection.subscribe_sync(client_id, [{"a/b", 0}])
      assert_receive {:subscription, :up, "a/b"}

      :ok = Tortoise.Connection.unsubscribe_sync(client_id, ["a/b"])
      assert_receive {:subscription, :down, "a/b"}

      :ok = Tortoise.publish(client_id, "a/b", "hello, world")
      refute_receive {:message, ["a", "b"], "hello, world"}, 200
      # check for typos
      refute_received _
    end

    test "wildcard unsubscribe", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: []
        )

      :ok = Tortoise.Connection.subscribe_sync(client_id, [{"a/b", 0}, {"a/c", 0}])
      assert_receive {:connection, :up}
      assert_receive {:subscription, :up, "a/b"}
      assert_receive {:subscription, :up, "a/c"}

      :ok = Tortoise.Connection.unsubscribe_sync(client_id, ["a/#"])
      assert_receive {:subscription, :down, "a/#"}

      :ok = Tortoise.publish_sync(client_id, "a/b", "hello, world(b)")
      :ok = Tortoise.publish_sync(client_id, "a/c", "hello, world(c)")
      refute_receive {:message, ["a", "b"], "hello, world(b)"}, 200
      refute_receive {:message, ["a", "c"], "hello, world(c)"}, 200
      # check for typos
      refute_received _
    end
  end

  describe "misc" do
    test "ping", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: []
        )

      assert_receive {:connection, :up}
      assert {:ok, _} = Tortoise.Connection.ping_sync(client_id, 1000)
    end

    test "disconnect", %{client_id: client_id, port: port} do
      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: client_id,
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: [],
          will: %Tortoise.Package.Publish{topic: "a/b", payload: "goodbye"}
        )

      {:ok, _pid} =
        Tortoise.Supervisor.start_child(
          client_id: "disconnect_test",
          handler: {IntegrationHandler, [pid: self()]},
          server: {Tortoise.Transport.Tcp, host: 'localhost', port: port},
          subscriptions: []
        )

      assert_receive {:connection, :up}
      assert_receive {:connection, :up}

      :ok = Tortoise.Connection.disconnect(client_id)
      assert_receive {:connection, :terminating}
      assert_receive {:terminate, :normal}

      assert_receive {:message, ["a", "b"], "goodbye"}
      refute_receive _
    end
  end
end
