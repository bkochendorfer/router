defmodule Router.Handler do
  require Lager
  require :cowboy_req, as: R

  @behaviour :cowboy_http_handler

  def init(_type, req, _opts) do
    {:ok, req, :undefine}
  end

  # If more than three backends respond down bubble up an error page
  def handle(req, state, acc) when acc >= 3 do
    {:ok, req} = R.reply(500, [], render_error_page , req)
  end

  def handle(req, state, acc \\ 0) do
    { _, url, headers, method, host, body, host_routed } = request_data(req)
    case route_request(url, method, headers, body) do
      {:ok, {{version, status, reasonphrase}, headers, body} } ->
         {:ok, req} = R.reply(status, headers, body, req)
      {:error, _error} ->
        set_bad_backend(host_routed)
        handle(req, state, acc + 1)
    end
    {:ok, req, state}
  end

  def terminate(_reason, _request, _state), do: :ok

  defp request_data(req) do
    { host, _req }      = R.host(req)
    { method, _ }       = R.method(req)
    { headers, _ }      = R.headers(req)
    { path, _ }         = R.path(req)
    { querystring, _ }  = R.qs(req)
    { _, body, _ }      = R.body(req)
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

  defp format_url(url) do
    case String.downcase(url) do
      <<"http://"::utf8, _::binary>> -> url
      <<"https://"::utf8, _::binary>> -> url
      _ ->
        {:ok, url} = "http://" <> url |> String.to_char_list
        url
    end
  end

  defp get_app_for(host, acc) when acc == 5 do
    {:ok, req} = R.reply(500, [], render_error_page)
  end

  defp get_app_for(host, acc \\ []) do
    routed_host = Redis.lrange(host, 0, -1) |> Enum.shuffle |> List.first
    case Cache.Client.bad?(routed_host) do
      :not_found -> routed_host
      :found -> get_app_for(host, acc)
    end
  end

  # TODO: Convert everything httpc with a helper or something
  defp route_request(url, method, headers, _body \\ "") when method == "GET" do
    # try do
      url2 = format_url(url)
      {:ok, {{version, status, reasonphrase}, headers, body}} = :httpc.request url2
      {:ok, {{version, status, reasonphrase}, headers, body}}
    # rescue
    # e ->
        #log_route_error url
        #{:error, "Unroutable"}
    #end
  end

  defp route_request(url, method, headers, body) when method == "POST" do
    try do
      url2 = format_url(url)
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

  def render_error_page do
    {:ok, file} = File.read("public/500.html")
    file
  end

  defp set_bad_backend(host) do
    Cache.Client.set(host)
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
