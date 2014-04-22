defmodule Router.Cache do

  def bad_backend? backend do
    Redis.get(backend)
  end

  def set_bad_backend backend do
    Redis.set(backend, "bad")
    Redis.expire(backend, 5)
  end

  def render_error_page do
    {:ok, file} = File.read("public/500.html")
    file
  end
end
