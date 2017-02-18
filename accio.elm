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
import UrlParser exposing (parseHash)


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
    { url : Maybe Input
    , errorMessage : String
    , token : Maybe String
    , spreadsheetUrl : String
    }


type Input
    = ApiUrl String
    | Json String


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model (OAuth.parseState location |> validateState) "" (OAuth.parseToken (Debug.log "location" location)) "", Navigation.modifyUrl "#" )

validateState : Maybe String -> Maybe Input
validateState state =
  case state of
    Just str ->
          if contains (regex "{") str then
              Just (Json str)
          else
              Just (ApiUrl (withDefault "error" (decodeUri str)))

    Nothing ->
      Nothing


-- UPDATE


type Msg
    = NoOp
    | Url String
    | GetData
    | Fetch (Result Http.Error String)
    | PostCsv (Result Http.Error String)
    | Error String


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

        Error msg ->
            ( { model | errorMessage = msg }, Cmd.none )


validateInput : String -> Maybe Input
validateInput str =
    if contains (regex "{") str then
        Just (Json str)
    else
        Just (ApiUrl str)


getData : Maybe Input -> Model -> Cmd Msg
getData input model =
    case input of
        Just (Json str) ->
            requestCsv model.token model (GoogleSheet.createSheet str)

        Just (ApiUrl str) ->
            getJson str

        Nothing ->
            Cmd.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "width", "800px" ), ( "margin", "0 auto" ) ] ]
        [ body
            []
            [ Html.header []
                [ h1 [ style [ ( "margin-bottom", "100px" ) ] ] [ text "Turn Json into a Google Sheet. Each object will become a row." ] ]
            , inputOrLink model
            ]
        ]

inputOrLink model =
  case model.spreadsheetUrl of
    "" ->
      div []
        [ p [] [ text "Enter your JSON or URL here:" ]
        , textarea [ placeholder "JSON or URL", rows 10, cols 70, style [ ( "display", "block" ) ], onInput Url ] [showUrl model]
        , authorizeOrConvert model
        ]
    url ->
       div [ ]
          [  a [ href url ] [ text "Click here to see your spreadsheet" ]
          ]

showUrl model =
  case model.url of
    Just (Json str) ->
        text str

    Just (ApiUrl str) ->
        text str

    Nothing ->
        text ""

authorizeOrConvert model =
  case model.token of
    Just token ->
      button [onClick GetData] [ text "Convert"]
    Nothing ->
      a [ href <| OAuth.requestToken <| Debug.log "url" <| packageState model.url ] [ text "Authorize Google" ]

packageState : Maybe Input -> String
packageState url =
    case url of
        Just (Json str) ->
            str

        Just (ApiUrl str) ->
            encodeUri str

        Nothing ->
          "#"



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
