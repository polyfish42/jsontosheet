port module Accio exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http exposing (..)
import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode
import Task
import Debug
import String
import Regex exposing (..)
import Array
import Platform.Cmd exposing (..)
import Parser exposing (..)
import Maybe exposing (..)
import Navigation
import Regex
import OAuth
import UrlParser exposing (parseHash, parsePath, s, (<?>), stringParam)
import Dict


main =
    Navigation.program
        (always NoOp)
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { url : String
    , errorMessage : String
    , keyValues : List (List (String, E.Value))
    , token : Maybe String
    , spreadsheetUrl : String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model "" "" [] (parseToken location) "", Cmd.none )


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


type JsonVal
    = JsonString String
    | JsonObject (Dict.Dict String JsonVal)
    | JsonFloat Float
    | JsonInt Int
    | JsonNull
    | JsonArray (List JsonVal)



-- UPDATE


type Msg
    = NoOp
    | Url String
    | GetData
    | Fetch (Result Http.Error String)
    | GetCsv
    | PostCsv (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Url url ->
            ( { model | url = url }, Cmd.none )

        GetData ->
            ( model, getJson model.url )

        Fetch (Ok response) ->
            ( { model | keyValues = createSheet response }, Cmd.none )

        Fetch (Err _) ->
            ( { model | errorMessage = toString Err }, Cmd.none )

        GetCsv ->
            ( model, requestCsv model.token )

        PostCsv (Ok response) ->
            ( { model | spreadsheetUrl = response }, Cmd.none )

        PostCsv (Err _) ->
            ( model, Cmd.none )


createSheet : String -> List (List ( String, E.Value ))
createSheet response =
    D.decodeString jsonDecoder response
        |> flattenAndEncode


jsonDecoder : Decoder JsonVal
jsonDecoder =
    D.oneOf
        [ D.map JsonString D.string
        , D.map JsonInt D.int
        , D.map JsonFloat D.float
        , D.list (D.lazy (\_ -> jsonDecoder)) |> D.map JsonArray
        , D.dict (D.lazy (\_ -> jsonDecoder)) |> D.map JsonObject
        , D.null JsonNull
        ]


flattenAndEncode : Result String JsonVal -> List (List ( String, E.Value ))
flattenAndEncode json =
    case json of
        Ok response ->
            case response of
                JsonObject obj ->
                    [ destructure [] "" (JsonObject obj) ]

                JsonArray list ->
                    List.map (destructure [] "") list

                _ ->
                    [ [ ( "error", E.string "irregular json" ) ] ]

        Err message ->
            [ [ ( "error", E.string message ) ] ]


destructure : List ( String, E.Value ) -> String -> JsonVal -> List ( String, E.Value )
destructure acc nestedName jsonVal =
    case jsonVal of
        JsonObject object ->
            case (Dict.toList object) of
                x :: xs ->
                    case x of
                        ( key, JsonString val ) ->
                            destructure (( nestKeys nestedName key, E.string val ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonInt val ) ->
                            destructure (( nestKeys nestedName key, E.int val ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonFloat val ) ->
                            destructure (( nestKeys nestedName key, E.float val ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonNull ) ->
                            destructure (( nestKeys nestedName key, E.null ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonArray list ) ->
                            destructure ((destructureArray nestedName key list [] 0) ++ acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonObject obj ) ->
                            (destructure acc "" (JsonObject (Dict.fromList xs))) ++ (destructure [] (nestKeys nestedName key) (JsonObject obj))

                x ->
                    case x of
                        [ ( key, JsonString val ) ] ->
                            ( nestKeys nestedName key, E.string val ) :: acc

                        [ ( key, JsonInt val ) ] ->
                            ( nestKeys nestedName key, E.int val ) :: acc

                        [ ( key, JsonFloat val ) ] ->
                            ( nestKeys nestedName key, E.float val ) :: acc

                        [ ( key, JsonNull ) ] ->
                            ( nestKeys nestedName key, E.null ) :: acc

                        [ ( key, JsonArray list ) ] ->
                            (destructureArray nestedName key list [] 0) ++ acc

                        [ ( key, JsonObject obj ) ] ->
                            destructure acc (nestKeys nestedName key) (JsonObject obj)

                        _ ->
                            acc

        _ ->
            [ ( "y", E.string "case" ) ]


nestKeys : String -> String -> String
nestKeys nestedNames key =
    case nestedNames of
        "" ->
            key

        str ->
            str ++ "/" ++ key


destructureArray : String -> String -> List JsonVal -> List ( String, E.Value ) -> Int -> List ( String, E.Value )
destructureArray nestedName key list acc counter =
    case list of
        x :: xs ->
            case x of
                JsonString str ->
                    destructureArray nestedName key xs (( (nestKeys nestedName key) ++ "/" ++ (toString counter), E.string str ) :: acc) (counter + 1)

                JsonInt int ->
                    destructureArray nestedName key xs (( (nestKeys nestedName key) ++ "/" ++ (toString counter), E.int int ) :: acc) (counter + 1)

                JsonNull ->
                    destructureArray nestedName key xs (( (nestKeys nestedName key) ++ "/" ++ (toString counter), E.null ) :: acc) (counter + 1)

                _ ->
                    [ ( "error", E.null ) ]

        [] ->
            acc



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ a [ href <| OAuth.requestToken ] [ text "Authorize Google" ]
        , a [ href model.spreadsheetUrl ] [ text "Click here to see your spreadsheet" ]
        , a [] [ text (toString (model.keyValues)) ]
        , input [ type_ "text", placeholder "url", onInput Url ] []
        , button [ onClick GetData ] [ text "Get Data" ]
        , button [ onClick GetCsv ] [ text "Create Google Sheet" ]
        ]



-- HTTP


getJson : String -> Cmd Msg
getJson url =
    Http.send Fetch <|
        Http.getString (url)


requestCsv : Maybe String -> Cmd Msg
requestCsv token =
    case token of
        Just token ->
            Http.send PostCsv (putRequest token)

        Nothing ->
            Cmd.none


putRequest : String -> Http.Request String
putRequest token =
    Http.request
        { method = "POST"
        , headers = [ getHeaders (Debug.log "token" token) ]
        , url = "https://sheets.googleapis.com/v4/spreadsheets"
        , body =
            Http.multipartBody
                [ Http.stringPart "spreadsheetID" ""
                , Http.stringPart "properties" """
                                { "title": "test"}
                                """
                , Http.stringPart "sheets" """
                                {"data":[{"startRow" :0}]}
                                """
                ]
        , expect = expectJson (D.field "spreadsheetUrl" D.string)
        , timeout = Nothing
        , withCredentials = False
        }


getHeaders : String -> Http.Header
getHeaders token =
    Http.header "Authorization" ("Bearer " ++ token)
