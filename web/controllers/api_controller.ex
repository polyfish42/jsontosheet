defmodule Accio.APIController do
  use Accio.Web, :controller

  def request(conn, %{"url" => url} = params) do

    case HTTPoison.get("#{url}") do
    {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
      res = body
    {:ok, %HTTPoison.Response{status_code: 404}} ->
      res = "Not found!"
    {:error, %HTTPoison.Error{reason: reason}} ->
      res = "The error is #{reason}"
    end

    render conn, "response.json", response: res
  end
end

# Poison.Encoder.encode(body, pretty: true)
