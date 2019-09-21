# Creep

[![CircleCI](https://circleci.com/gh/ConnorRigby/creep.svg?style=svg)](https://circleci.com/gh/ConnorRigby/creep)
[![Coverage Status](https://coveralls.io/repos/github/ConnorRigby/creep/badge.svg?branch=master)](https://coveralls.io/github/ConnorRigby/creep?branch=master)
[![Hex version](https://img.shields.io/hexpm/v/creep.svg "Hex version")](https://hex.pm/packages/creep)

Highly experimental Pure Elixir MQTT Broker.

[target specification is 3.1.1](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/csprd02/mqtt-v3.1.1-csprd02.html)
some things work.

## Known issues

* Only QOS 0 is currently supported
* Session management should be backed in ETS or something probably
* wildcards are only partially implemented
  * `#` works
  * `+` does not
* packet parse errors should probably be cleaned up
* performance might not be great?

## Why the name

[a collection of tortoises is called a creep](http://mentalfloss.com/article/56805/16-fun-facts-about-tortoises)