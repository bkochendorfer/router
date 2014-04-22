defmodule Router do
  use Application.Behaviour

  @doc "Starts the router. Cowboy router takes a function and routes requests
  through that function"

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([
      {:_, [
        {:_, Router.Routes, []}
      ]}
    ])

    :cowboy.start_http(
      :http, 6000, [ip: {127,0,0,1}, port: 9080], [env: [dispatch: dispatch]]
    )
    Router.Supervisor.start_link
  end
end
