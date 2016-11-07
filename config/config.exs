# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :accio,
  ecto_repos: [Accio.Repo]

# Configures the endpoint
config :accio, Accio.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "0tdd1rxnjAk3qLMHkIv7yhld9ImdiUm4jpxf5Z36Zvq7CQO9Dd9E7HoSKWV9j5I8",
  render_errors: [view: Accio.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Accio.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
