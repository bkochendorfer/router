defmodule Router do
  use Application.Behaviour

  @doc "Starts the router. Cowboy router takes a function and routes requests
  through that function"

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([{:_, [ {:_, Router.Handler, []}]}])

    :cowboy.start_http(
      :http, 100,
      [ip: {127,0,0,1}, port: 9080],
      [env: [dispatch: dispatch]]
    )

    # The client should maybe be it's own OTP app as well
    Cache.Client.start
    Redis.start_link
    Router.Supervisor.start_link
  end
end
