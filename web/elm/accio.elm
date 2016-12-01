port module Accio exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Json
import Json.Encode
import Task
import Debug
import String
import Regex exposing (..)
import Array
import Platform.Cmd exposing (..)

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
  , uid : Int
  , selected : Bool
  , responses : String
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
newKeyValue input id =
    { key = String.dropLeft 5 input
    , value = "empty"
    , selected = False
    , id = id
    , indent =
      (Debug.log "left" (String.left 5 input))
        |> String.toInt
        |> Result.withDefault 0
    }

init : (Model, Cmd Msg)
init =
  ( Model "" "" 0 False "" [], Cmd.none )


-- UPDATE

type Msg
  = Add String
  | Url String
  | GetData
  | Fetch (Result Http.Error String)
  | Select Int

port format : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Add response ->
        ({ model
        | uid = model.uid + 1
        , keyValues = List.append model.keyValues (enterKeyValues response)
        }, Cmd.none)

    Url url ->
      ({ model | url = url }, Cmd.none)

    GetData ->
      (model, getJson model.url)

    Fetch (Ok response) ->
      (model, format response)

    Fetch (Err _) ->
      ({model | errorMessage = toString Err}, Cmd.none)

    Select id ->
      let
        updateSelected t =
          if t.id == id then
            { t | selected = not t.selected}
          else
            t
      in
      ({model | keyValues = List.map updateSelected model.keyValues}, Cmd.none)

enterKeyValues : String -> List KeyValue
enterKeyValues response =
  -- List.map newKeyValue (parseJson response)

  listKeyValues (parseJson response) 1 []

listKeyValues : (List String) -> Int -> List KeyValue -> List KeyValue
listKeyValues response uid acc =
  case response of
    [] -> List.reverse acc

    x::xs ->
      listKeyValues xs (uid +1) ((newKeyValue x uid)::acc)
  -- call newkeyvalue recrusively through the list while incrementing the id


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
    , section [] [ viewKeyValues model.keyValues ]
    ]


viewKeyValues : List KeyValue -> Html Msg
viewKeyValues keyValues =
        section
            [ class "main" ]
            [ Keyed.ul [] <|
                List.map viewKeyedResponse keyValues
            ]

viewKeyedResponse : KeyValue -> ( String, Html Msg )
viewKeyedResponse keyValue =
    ( toString keyValue.id, lazy viewLine keyValue )

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
      , onClick (Select keyValue.id)
      ]
      [ text keyValue.key ]

-- VIEW HELPERS

px : Int -> String
px int =
  toString int
  ++ "px"

splitLine : String -> (Int, String)
splitLine line =
  let
    indent =
      String.left 5 line
      |> String.toInt
      |> Result.withDefault 0
    newLine =
      String.dropLeft 5 line
  in
    (indent, newLine)

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
    -- Http.getString(url)


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
