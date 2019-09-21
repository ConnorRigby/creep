defmodule Creep.MixProject do
  use Mix.Project

  def project do
    [
      app: :creep,
      version: "0.1.0-alpha",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      deps: deps(),
      package: package()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      start: {Creep.Application, []}
    ]
  end

  def package do
    [
      description: "WIP Experimental MQTT broker built in pure Elixir",
      licenses: ["ISC"],
      links: %{
        "GitHub" => "https://github.com/ConnorRigby/creep",
        "MQTT 3.1.1 spec" =>
          "http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/csprd02/mqtt-v3.1.1-csprd02.html",
        "Tortoise" => "https://github.com/gausby/tortoise"
      }
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ranch, "~> 2.0-rc.1"},
      {:tortoise, "~> 0.9.4", only: [:test, :dev]},
      {:excoveralls, "~> 0.10", only: :test},
      {:dialyxir, "1.0.0-rc.6", only: [:test, :dev], runtime: false},
      {:ex_doc, "~> 0.21.2", only: [:test, :dev]}
    ]
  end
end
