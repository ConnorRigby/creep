defmodule Creep.MixProject do
  use Mix.Project

  def project do
    [
      app: :creep,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      start: {Creep.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ranch, "~> 2.0-rc.1"},
      {:excoveralls, "~> 0.10", only: :test},
      {:tortoise, "~> 0.9.4", only: [:test, :dev]}
    ]
  end
end
