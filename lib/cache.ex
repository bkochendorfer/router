defmodule Cache do
  use Application.Behaviour

  @doc "Starts a cache for the router"
  def start(_type, _args) do
    Cache.supervisor.start_link
  end
end
