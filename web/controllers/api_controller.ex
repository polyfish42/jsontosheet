defmodule Accio.APIController do
  use Accio.Web, :controller

  def response(conn, _params) do
    render conn, "response.html"
  end
end
