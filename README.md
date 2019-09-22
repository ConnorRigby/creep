# Creep

[![CircleCI](https://circleci.com/gh/ConnorRigby/creep.svg?style=svg)](https://circleci.com/gh/ConnorRigby/creep)
[![Coverage Status](https://coveralls.io/repos/github/ConnorRigby/creep/badge.svg?branch=master)](https://coveralls.io/github/ConnorRigby/creep?branch=master)
[![Hex version](https://img.shields.io/hexpm/v/creep.svg "Hex version")](https://hex.pm/packages/creep)

Highly experimental Pure Elixir MQTT Broker.

[target specification is currently 3.1.1](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/csprd02/mqtt-v3.1.1-csprd02.html)
some things work.

## Usage

The following is heavily subject to change.

From IEx:

```elixir
broker_opts = [
    broker_id: "my_broker",
    packet_processor: Creep.InMemProcessor,
    transports: [
        {Creep.RanchTransport, [port: 1883]},
        {Creep.PlugTransport, [port: 4000]}
    ]
]
{:ok, broker_pid} = Creep.start_link(broker_opts)

client_opts = [
    client_id: "my_client",
    handler: {Tortoise.Handler.Logger, []},
    server: {Tortoise.Transport.Tcp, host: 'localhost', port: 1883},
    subscriptions: [{"testtopic/1", 0}]
]
{:ok, client_pid} = Tortoise.Supervisor.start_child(client_opts)
Tortoise.publish_sync("my_client", "testtopic/1", "hello, from TCP")
```

From `config.exs`:

```elixir
use Mix.Config

config :creep, Creep.Application,
  brokers: [
    [
      broker_id: "my_broker",
      packet_processor: Creep.InMemProcessor,
      transports: [
          {Creep.RanchTransport, [port: 1883]},
          {Creep.PlugTransport, [port: 4000]}
      ]
    ]
  ]
```

SSL:

```elixir
ssl_broker_opts = [
  broker_id: "my_broker",
  packet_processor: Creep.InMemProcessor,
  transports: [
    {Creep.RanchTransport,
      [
        port: 8883,
        ssl: true,
        cacertfile: '/home/connor/oss/elixir/creep/ssl/ca.crt',
        certfile: '/home/connor/oss/elixir/creep/ssl/server.crt',
        keyfile: '/home/connor/oss/elixir/creep/ssl/server.key'
      ]},
    {Creep.RanchTransport, [port: 1883]},
    {Creep.PlugTransport, [port: 4000]}
  ]
]
{:ok, ssl_broker_pid} = Creep.start_link(ssl_broker_opts)

ssl_client_opts = [
  client_id: "smart-spoon",
  handler: {Tortoise.Handler.Logger, []},
  server: {
    Tortoise.Transport.SSL,
    host: 'localhost', port: 8883,
    cacertfile: '/home/connor/oss/elixir/creep/ssl/ca.crt',
    certfile: '/home/connor/oss/elixir/creep/ssl/server.crt',
    keyfile: '/home/connor/oss/elixir/creep/ssl/server.key'
  },
  subscriptions: [{"foo/bar", 0}])
]
{:ok, ssl_client_pid} = Tortoise.Supervisor.start_child(ssl_client_opts)
```

## Progress

- [X] 3.1.1 packet decode/encode
- [ ] 5.0.0 packet decode/encode
- [X] TCP transport
- [X] SSL transport
- [X] WebSocket transport (http)
- [ ] WebSocket transport (https)
- [ ] better Plug integration?
- [X] Publish/Subscribe QOS 0
- [ ] Publish/Subscribe QOS 1
- [ ] Publish/Subscribe QOS 2
- [ ] Extension API
- [ ] Session management
- [ ] Benchmarks

## Why the name

[a collection of tortoises is called a creep](http://mentalfloss.com/article/56805/16-fun-facts-about-tortoises)