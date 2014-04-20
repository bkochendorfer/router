defmodule Router.Mixfile do
  use Mix.Project

  def project do
    [ app: :router,
      version: "0.0.1",
      elixir: "~> 0.12.5",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { Router, [] },
      applications: [ :cowboy, :exlager ]
    ]
  end

  defp deps do
    [
      {:httpotion, "0.2.3", github: "myfreeweb/httpotion"},
      {:cowboy, "~> 0.9", github: "extend/cowboy"},
      {:redis, "~> 1.1.0", github: "timbuchwaldt/elixir-redis"},
      {:exlager, ref: "2a4b002dfe34abf1b03c9d26a3ebe2e101437f51", github: "khia/exlager"}
    ]
  end
end
