defmodule Accio.APIController do
  use Accio.Web, :controller

  def request(conn, _params) do
    key = conn.params["parameters"]["key"]
    secret = "#{conn.params["parameters"]["secret"]}"
    survey_id = conn.params["parameters"]["survey_id"]

    case HTTPoison.get("https://#{key}:#{secret}@api.qualaroo.com/api/v1/nudges/#{survey_id}/responses.json") do
    {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
      res = Poison.encode!(body, pretty: true)
    {:ok, %HTTPoison.Response{status_code: 404}} ->
      res = "Not found!"
    {:error, %HTTPoison.Error{reason: reason}} ->
      res = "The error is #{reason}"
    end

    render conn, "response.html", response: res
  end
end
