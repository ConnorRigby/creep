defmodule Creep.PlugTransport.MQTTRouter do
  use Plug.Router
  use Plug.Debugger, otp_app: :creep
  plug(Plug.Logger)
  plug(:match)
  plug(:dispatch)

  match(_, do: send_resp(conn, 404, "Page not found"))
end
