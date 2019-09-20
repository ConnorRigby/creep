defmodule Creep.Packet.Util do
  @moduledoc "Utility fucntions for encoding/decoding"

  def bool(1), do: true
  def bool(0), do: false
  def bool(true), do: 1
  def bool(false), do: 0
end
