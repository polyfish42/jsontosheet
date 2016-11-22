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
  , field : String
  , uid : Int
  , selected : Bool
  , responses : List Response
  }

type alias Response =
  { text : String
  , selected : Bool
  , id : Int
  }


newResponse : String -> Int -> Response
newResponse str id =
  { text = str
  , selected = False
  , id = id
  }

init : (Model, Cmd Msg)
init =
  ( Model "" "" "" 0 False [], Cmd.none )


-- UPDATE

type Msg
  = Url String
  | Add String
  | UpdateField String
  | GetData
  | Fetch (Result Http.Error String)
  | Select Int

port format : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Add json ->
      ({ model
        | uid = model.uid + 1
        , field = json
        , responses = model.responses ++ [ newResponse json model.uid ]
      }
      , Cmd.none
      )

    UpdateField str ->
      ({ model | field = str }, Cmd.none)

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
      ({model | responses = List.map updateSelected model.responses}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "url", onInput Url ] []
    , button [ onClick GetData ] [ text "Get Data"]
    , section [] [ viewResponses model.responses ]
    ]


viewResponses : List Response -> Html Msg
viewResponses responses =
        section
            [ class "main" ]
            [ Keyed.ul [] <|
                List.map viewKeyedResponse responses
            ]

viewKeyedResponse : Response -> ( String, Html Msg )
viewKeyedResponse response =
    ( toString response.id, lazy viewResponse response )

viewResponse : Response -> Html Msg
viewResponse response =
    -- p [ classList [ ("selected", property.selected)
    --               , ("unselected", property.selected == False)
    --               ]
    --   , onClick (Select property.id)
    --   ]
    section [] [parseJson response.text]

parseJson : String -> Html msg
parseJson json =
  let
    lines =
      json
      |> formatString "" False 0
      |> String.split uniqueHead
  in
  div [] <| List.map viewLine lines

viewLine : String -> Html msg
viewLine lineStr =
  let
    (indent, lineTxt) = splitLine lineStr
  in
    p [style
        [ ("paddingLeft", px (indent))
        , ("marginTop", "0px")
        , ("marginBottom", "0px")
        ]
      ]
      [ text lineTxt ]

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
port javascriptValues : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  javascriptValues Add



-- HTTP


getJson : String -> Cmd Msg
getJson url =
  Http.send Fetch <|
    Http.getString("http://localhost:4000/response?url=" ++ url)
  -- Task.perform FetchFail (FetchSucceed (Http.get("http://localhost:4000/response?url=" ++ url)))

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
