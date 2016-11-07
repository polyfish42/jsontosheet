defmodule Accio.PageController do
  use Accio.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
