module Accio exposing (..)

import Array
import Debug
import Dict
import GoogleSheet
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)
import Maybe exposing (..)
import Navigation
import OAuth
import Platform.Cmd exposing (..)
import Regex exposing (..)
import String


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
    { url : Input
    , errorMessage : String
    , token : Maybe String
    , spreadsheetUrl : String
    }

type Input
    = ApiUrl String
    | Json String

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model (ApiUrl "") "" (OAuth.parseToken location) "", Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Url String
    | GetData
    | Fetch (Result Http.Error String)
    | PostCsv (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Url url ->
            ( { model | url = validateInput url }, Cmd.none )

        GetData ->
            ( model, getData model.url model )

        Fetch (Ok response) ->
            ( model, requestCsv model.token model (GoogleSheet.createSheet response) )

        Fetch (Err _) ->
            ( { model | errorMessage = toString Err }, Cmd.none )

        PostCsv (Ok response) ->
            ( { model | spreadsheetUrl = response }, Cmd.none )

        PostCsv (Err _) ->
            ( model, Cmd.none )

validateInput : String -> Input
validateInput str =
  if contains (regex "{") str then
    Json str
  else
    ApiUrl str

getData : Input -> Model -> Cmd Msg
getData input model =
  case input of
    Json str ->
      requestCsv model.token model (GoogleSheet.createSheet str)
    ApiUrl str ->
      getJson str


-- VIEW


view : Model -> Html Msg
view model =
    ul [ style [ ( "list-style", "none" ) ] ]
        [ li []
            [ text "Step 1: "
            , a [ href <| OAuth.requestToken ] [ text "Authorize Google" ]
            ]
        , li []
            [ text "Step 2: Enter JSON or URL here "
            , input [ type_ "text", placeholder "JSON or URL", onInput Url ] []
            , button [ onClick GetData ] [ text "Create Sheet" ]
            ]
        , li []
            [ text "Step 3: "
            , a [ href model.spreadsheetUrl ] [ text "Click here to see your spreadsheet" ]
            ]
        ]



-- HTTP


getJson : String -> Cmd Msg
getJson url =
    Http.send Fetch <|
        Http.getString (url)


requestCsv : Maybe String -> Model -> E.Value -> Cmd Msg
requestCsv token model requestBody =
    case token of
        Just token ->
            Http.send PostCsv (putRequest token model requestBody)

        Nothing ->
            Cmd.none


putRequest : String -> Model -> E.Value -> Http.Request String
putRequest token model requestBody =
    Http.request
        { method = "POST"
        , headers = [ getHeaders token ]
        , url = "https://sheets.googleapis.com/v4/spreadsheets"
        , body = Http.jsonBody requestBody
        , expect = expectJson (D.field "spreadsheetUrl" D.string)
        , timeout = Nothing
        , withCredentials = False
        }


getHeaders : String -> Http.Header
getHeaders token =
    Http.header "Authorization" ("Bearer " ++ token)
