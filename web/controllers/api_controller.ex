defmodule Accio.APIController do
  use Accio.Web, :controller

  def request(conn, %{"url" => url}) do
    res =
      case HTTPoison.get("#{url}") do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        # Poison.decode!(body)
        body
      {:ok, %HTTPoison.Response{status_code: 404}} ->
        "Not found!"
      {:error, %HTTPoison.Error{reason: reason}} ->
        "The error is #{reason}"
      end

    render conn, "response.json", response: res
  end
end
