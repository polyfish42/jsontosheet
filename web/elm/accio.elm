port module Accio exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Task
import Debug
import String
import Regex exposing (..)
import Array
import Platform.Cmd exposing (..)
import Parser exposing (..)
import Maybe exposing (..)
import OAuth
import OAuth.Config

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { url : String
  , errorMessage : String
  , keyValues : List KeyValue
  }

type alias KeyValue =
  { key : String
  , value : String
  , selected : Bool
  , id : Int
  , indent : Int
  }


newKeyValue : String -> Int -> KeyValue
newKeyValue str id =
    case String.split "\":" str of
      [key, value] ->
        { key = (String.dropLeft 5 key) ++ "\""
        , value = value
        , selected = False
        , id = id
        , indent =
          String.left 5 str
            |> String.toInt
            |> Result.withDefault 0
        }
      char::xs ->
        { key = String.dropLeft 5 char
        , value = ""
        , selected = False
        , id = id
        , indent =
          String.left 5 str
            |> String.toInt
            |> Result.withDefault 0
        }
      [] ->
        { key = "something"
        , value = " is worng"
        , selected = False
        , id = id
        , indent = 1
        }

init : (Model, Cmd Msg)
init =
  ( Model "" "" [], Cmd.none )


-- UPDATE

type Msg
  = Add String
  | Url String
  | GetData
  | Fetch (Result Http.Error String)
  | Select String
  | GetCsv
  | PostCsv (Result Http.Error ())

port format : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Add response ->
        ({ model
        | keyValues = List.append model.keyValues (enterKeyValues response)
        }, Cmd.none)

    Url url ->
      ({ model | url = url }, Cmd.none)

    GetData ->
      (model, getJson model.url)

    Fetch (Ok response) ->
      (model, format response)

    Fetch (Err _) ->
      ({model | errorMessage = toString Err}, Cmd.none)

    Select key ->
      let
        updateSelected t =
          if t.key == key then
            { t | selected = not t.selected}
          else
            t
      in
      ({model | keyValues = List.map updateSelected model.keyValues}, Cmd.none)

    GetCsv ->
      (model, requestCsv)

    PostCsv (Ok response) ->
      (model, Cmd.none)

    PostCsv (Err _) ->
      (model, Cmd.none)

enterKeyValues : String -> List KeyValue
enterKeyValues response =
  formatKeyValues (parseJson response) 1 []

formatKeyValues : (List String) -> Int -> List KeyValue -> List KeyValue
formatKeyValues response uid acc =
  case response of
    [] -> List.reverse acc

    x::xs ->
      formatKeyValues xs (uid +1) ((newKeyValue x uid)::acc)

parseJson : String -> (List String)
parseJson json =
      json
      |> formatString "" False 0
      |> String.split uniqueHead

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "url", onInput Url ] []
    , button [ onClick GetData ] [ text "Get Data"]
    , button [ onClick GetCsv ] [ text "Create Google Sheet"]
    , section [] [ viewKeyValues model.keyValues ]
    ]


viewKeyValues : List KeyValue -> Html Msg
viewKeyValues keyValues =
        section
            [ class "main" ]
            [ Keyed.ul [] <|
                List.map viewKeyedLi keyValues
            ]

viewKeyedLi : KeyValue -> ( String, Html Msg )
viewKeyedLi keyValue =
    ( toString keyValue.id, Html.Lazy.lazy viewLine keyValue )

viewLine : KeyValue -> Html Msg
viewLine keyValue =
    p [ classList
        [("selected", keyValue.selected)
        ,("unselected", keyValue.selected == False)
        ]
      , style
        [ ("paddingLeft", px (keyValue.indent))
        , ("marginTop", "0px")
        , ("marginBottom", "0px")
        ]
      , onClick (Select keyValue.key)
      ]
      [ text (keyValue.key ++ ":" ++ keyValue.value) ]

-- VIEW HELPERS

px : Int -> String
px int =
  toString int
  ++ "px"

-- SUBSCRIPTIONS

port stringyfiedJson : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
     stringyfiedJson Add


-- HTTP

getJson : String -> Cmd Msg
getJson url =
  Http.send Fetch <|
    Http.getString("http://localhost:4000/response?url=" ++ url)

requestCsv : Cmd Msg
requestCsv =
  Http.send PostCsv putRequest

putRequest : Http.Request ()
putRequest =
  Http.request
        { method = "PUT"
        , headers = []
        , url = "https://sheets.googleapis.com/v4/spreadsheets/1EQzBkARU0V7vMbZaRu8u-iz9TxmcGX-dG-7lYnGqNlY/values/Sheet1!A1%3AE5?includeValuesInResponse=true&responseDateTimeRenderOption=SERIAL_NUMBER&responseValueRenderOption=FORMATTED_VALUE&valueInputOption=RAW&key=1EQzBkARU0V7vMbZaRu8u-iz9TxmcGX-dG-7lYnGqNlY"
        , body = Http.multipartBody
                      [ Http.stringPart "ValueRange" """
                                { "range": "Sheet1!A1:D5",
                                  "majorDimension": "ROWS",
                                  "values": [
                                    ["Item", "Cost", "Stocked", "Ship Date"],
                                    ["Wheel", "$20.50", "4", "3/1/2016"],
                                    ["Door", "$15", "2", "3/15/2016"],
                                    ["Engine", "$100", "1", "30/20/2016"],
                                    ["Totals", "=SUM(B2:B4)", "=SUM(C2:C4)", "=MAX(D2:D4)"]
                                  ],
                                }
                                """
                      , Http.stringPart "majorDimension" "ROWS"]
        , expect = expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }

-- PARSER

quote = "\""
indentChars = "{["
outdentChars = "}]"
newLineChars = ","
uniqueHead = "##FORMAT##"
incr = 20

formatString : String -> Bool -> Int -> String -> String
formatString acc isInQuotes indent str =
  case String.left 1 str of
    "" -> acc

    firstChar ->
      if isInQuotes then
        if firstChar == quote then
          formatString (acc ++ firstChar) (not isInQuotes) indent (String.dropLeft 1 str)
        else
          formatString (acc ++ firstChar) isInQuotes indent (String.dropLeft 1 str)
      else
        if String.contains firstChar newLineChars then
          formatString (acc ++ firstChar ++ uniqueHead ++ pad indent) isInQuotes indent (String.dropLeft 1 str)
        else if String.contains firstChar indentChars then
          formatString (acc ++ uniqueHead ++ pad (indent + incr) ++ firstChar) isInQuotes (indent + incr) (String.dropLeft 1 str)
        else if String.contains firstChar outdentChars then
          formatString (acc ++ firstChar ++ uniqueHead ++ pad (indent - incr)) isInQuotes (indent - incr) (String.dropLeft 1 str)
        else if firstChar == quote then
          formatString (acc ++ firstChar) (not isInQuotes) indent (String.dropLeft 1 str)
        else
          formatString (acc ++ firstChar) isInQuotes indent (String.dropLeft 1 str)

pad : Int -> String
pad indent =
  String.padLeft 5 '0' <| toString indent
