module OAuth exposing (..)

import Http exposing (..)

requestToken : String
requestToken =
  formUrl

formUrl : String
formUrl =
  url
    "https://accounts.google.com/o/oauth2/v2/auth"
    [ ("response_type", "token")
    , ("client_id", "591745061791-69jpb1uina8sp60eq8c0125dm5nb5hhd.apps.googleusercontent.com")
    , ("redirect_uri", "http://localhost:4000")
    , ("scope", "https://www.googleapis.com/auth/spreadsheets")
    , ("prompt", "consent")
    ]

url : String -> List (String, String) -> String
url endPoint args =
  case args of
    [] ->
        endPoint

    _ ->
        endPoint ++ "?" ++ String.join "&" (List.map queryPair args)

queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    Http.encodeUri key ++ "=" ++ Http.encodeUri value





-- request token (authenticate user, buildurl)
-- validate token
-- call api