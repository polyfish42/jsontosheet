module OAuth exposing (..)

import Dict exposing (..)
import Http exposing (..)
import Navigation
import UrlParser exposing (parseHash)


parseToken : Navigation.Location -> Maybe String
parseToken location =
    case (parseHash (UrlParser.string) location) of
        Just str ->
            str
                |> String.split "&"
                |> List.filterMap toKeyValuePair
                |> Dict.fromList
                |> Dict.get "access_token"

        Nothing ->
            Nothing


validateUrl : String -> String
validateUrl token =
  url "https://www.googleapis.com/oauth2/v3/tokeninfo"
        [("access_token", token)]

parseState : Navigation.Location -> Maybe String
parseState location =
    case (parseHash (UrlParser.string) location) of
        Just str ->
            str
                |> String.split "&"
                |> List.filterMap toKeyValuePair
                |> Dict.fromList
                |> Dict.get "state"

        Nothing ->
            Nothing


toKeyValuePair : String -> Maybe ( String, String )
toKeyValuePair segment =
    case String.split "=" segment of
        [ key, value ] ->
            Maybe.map2 (,) (Http.decodeUri key) (Http.decodeUri value)

        _ ->
            Nothing


requestToken : String -> String
requestToken state =
    formUrl state


formUrl : String -> String
formUrl state =
    url
        "https://accounts.google.com/o/oauth2/v2/auth"
        [ ( "response_type", "token" )
        , ( "client_id", "591745061791-69jpb1uina8sp60eq8c0125dm5nb5hhd.apps.googleusercontent.com" )
        , ( "redirect_uri", "http://jsontosheet.com/" )
        , ( "scope", "https://www.googleapis.com/auth/spreadsheets" )
        , ( "prompt", "consent" )
        , ( "state", state )
        ]


url : String -> List ( String, String ) -> String
url endPoint args =
    case args of
        [] ->
            endPoint

        _ ->
            endPoint ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    Http.encodeUri key ++ "=" ++ Http.encodeUri value
