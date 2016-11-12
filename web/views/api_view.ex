defmodule Accio.APIView do
  use Accio.Web, :view

  def render("response.json", %{response: res}) do
    res
  end
end
