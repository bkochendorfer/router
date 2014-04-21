defmodule Router.Routes do
  require Lager

  @behaviour :cowboy_http_handler

  def init(_type, req, _opts) do
    Redis.start
    HTTPotion.start
    {:ok, req, :undefine}
  end

  def handle(req, state) do
    { _, url, headers, method, host, body, host_routed } = request_data(req)
    case route_request(url, method, headers, body) do
      {:ok, response} ->
        {:ok, req} = :cowboy_req.reply(response.status_code, [], response.body, req)
      {:error, _error} ->
        set_bad_backend(host_routed)
        {:ok, error} = Router.Cache.render_error_page
        {:ok, req} = :cowboy_req.reply(500, [], error , req)
      end
    {:ok, req, state}
  end

  def terminate(_reason, _request, _state), do: :ok

  defp set_bad_backend(host) do
    Router.Cache.set_bad_backend(host)
  end

  defp request_data(req) do
    { host, _req } = :cowboy_req.host(req)
    { method, _ }  = :cowboy_req.method(req)
    { headers, _ } = :cowboy_req.headers(req)
    { path, _ } = :cowboy_req.path(req)
    { querystring, _ } = :cowboy_req.qs(req)
    { _, body, _ } = :cowboy_req.body(req)
    host_routed = get_app_for(host)
    url = build_url(host_routed, path, querystring)
    log "Routing #{host} to #{url}"
    {:ok, url, headers, method, host, body, host_routed }
  end

  defp build_url(host, path, querystring) when querystring == "/" or querystring == "" do
    host <> path
  end

  defp build_url(host, path, querystring) do
    host <> path <> "?" <> querystring
  end

  defp get_app_for(host) do
    routed_host = Redis.lrange(host, 0, -1) |> Enum.shuffle |> List.first
    case Router.Cache.bad_backend?(routed_host) do
      :undefined -> routed_host
      _ -> get_app_for(host)
    end
  end

  defp route_request(url, method, headers, body \\ "") when method == "GET" do
    try do
      resp = HTTPotion.get url, headers
      {:ok, resp}
    rescue
      e ->
        log_route_error url
        {:error, "Unroutable"}
    end
  end

  defp route_request(url, method, headers, body) when method == "POST" do
    try do
      resp = HTTPotion.post url, body, headers, [timeout: 1000]
      {:ok, resp}
    rescue
      e ->
        log_route_error url
        {:error, "Unroutable"}
    end
  end

  defp route_request(url, method, headers, body) when method == "PUT" do
    try do
      resp = HTTPotion.put url, body, headers, [timeout: 1000]
      {:ok, resp}
    rescue
      e ->
        log_route_error url
        {:error, "Unroutable"}
    end
  end

  defp log(log_line) do
    Lager.info log_line
  end

  defp log_error(log_line) do
    Lager.error log_line
  end

  defp log_route_error(url) do
    Lager.error "Unable to route to backend : #{url}"
  end

end
