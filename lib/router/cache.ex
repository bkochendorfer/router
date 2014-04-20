defmodule Router.Cache do
  def render_error_page do
    File.read("public/500.html")
  end
end
