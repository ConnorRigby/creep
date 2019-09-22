# Creep

[![CircleCI](https://circleci.com/gh/ConnorRigby/creep.svg?style=svg)](https://circleci.com/gh/ConnorRigby/creep)
[![Coverage Status](https://coveralls.io/repos/github/ConnorRigby/creep/badge.svg?branch=master)](https://coveralls.io/github/ConnorRigby/creep?branch=master)
[![Hex version](https://img.shields.io/hexpm/v/creep.svg "Hex version")](https://hex.pm/packages/creep)

Highly experimental Pure Elixir MQTT Broker.

[target specification is 3.1.1](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/csprd02/mqtt-v3.1.1-csprd02.html)
some things work.

## Usage

The following is heavily subject to change.

```elixir
iex()> {:ok, broker_pid} = Creep.start_link([broker_id: "my_broker", transport: Creep.RanchTransport, transport_opts: [port: 1883]])
{:ok, #PID<0.273.0>}

iex()> {:ok, client_pid} = Tortoise.Supervisor.start_child(
    client_id: "my_client",
    handler: {Tortoise.Handler.Logger, []},
    server: {Tortoise.Transport.Tcp, host: 'localhost', port: 1883},
    subscriptions: [{"foo/bar", 0}])
{:ok, #PID<0.299.0>}
19:47:14.791 [info]  Initializing handler

19:47:14.791 [info]  Connection has been established

19:47:14.799 [info]  Subscribed to foo/bar

iex()> Tortoise.publish_sync("my_client", "foo/bar", "hello, world")
:ok
19:47:38.804 [info]  foo/bar "hello, world"
iex()>
```

## Known issues

* Only QOS 0 is currently supported
* Session management should be backed in ETS or something probably
* wildcards are only partially implemented
  * `#` works
  * `+` does not
* packet parse errors should probably be cleaned up
* performance might not be great?
* only TCP is currently supported
  * SSL and Websockets should be supported

## Why the name

[a collection of tortoises is called a creep](http://mentalfloss.com/article/56805/16-fun-facts-about-tortoises)