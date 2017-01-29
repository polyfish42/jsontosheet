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


toKeyValuePair : String -> Maybe ( String, String )
toKeyValuePair segment =
    case String.split "=" segment of
        [ key, value ] ->
            Maybe.map2 (,) (Http.decodeUri key) (Http.decodeUri value)

        _ ->
            Nothing


requestToken : String
requestToken =
    formUrl


formUrl : String
formUrl =
    url
        "https://accounts.google.com/o/oauth2/v2/auth"
        [ ( "response_type", "token" )
        , ( "client_id", "591745061791-69jpb1uina8sp60eq8c0125dm5nb5hhd.apps.googleusercontent.com" )
        , ( "redirect_uri", "http://localhost:8000/accio.elm" )
        , ( "scope", "https://www.googleapis.com/auth/spreadsheets" )
        , ( "prompt", "consent" )
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
