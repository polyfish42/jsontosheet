defmodule Accio.Router do
  use Accio.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", Accio do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    # get "/response", APIController, :request
  end

  scope "/", Accio do
    pipe_through :api

    get "/response", APIController, :request
    get "/csv", CsvController, :export
  end

  # Other scopes may use custom stacks.
  # scope "/api", Accio do
  #   pipe_through :api
  # end
end
