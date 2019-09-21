defmodule Creep do
  def connection_test do
    Tortoise.Supervisor.start_child(
      client_id: "my_client_id",
      handler: {Tortoise.Handler.Logger, []},
      server: {Tortoise.Transport.Tcp, host: 'localhost', port: 1883},
      subscriptions: [{"a/b", 2}, {"c/d", 0}]
    )
  end

  def publish_test do
    Tortoise.publish("my_client_id", "foo/bar", "Hello from the World of Tomorrow !", qos: 2)
  end
end
