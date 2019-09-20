defmodule CreepTest do
  use ExUnit.Case
  doctest Creep

  test "greets the world" do
    assert Creep.hello() == :world
  end
end
