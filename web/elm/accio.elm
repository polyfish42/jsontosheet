port module Accio exposing (..)

import Html exposing (..)
import Html.App as App
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
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { url : String
  , response : String
  , field : String
  , uid : Int
  , selected : Bool
  , properties : List Property
  }

type alias Property =
  { value : String
  , selected : Bool
  , id : Int
  }

newProperty : String -> Int -> Property
newProperty entry id =
  { value = entry
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
  | FetchSucceed String
  | FetchFail Http.Error
  | Select Int

port format : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Add json ->
      ({ model
        | uid = model.uid + 1
        , field = json
        , properties = model.properties ++ [ newProperty json model.uid ]
      }
      , Cmd.none
      )

    UpdateField str ->
      ({ model | field = str }, Cmd.none)

    Url url ->
      ({ model | url = url }, Cmd.none)

    GetData ->
      (model, getJson model.url)

    FetchSucceed response ->
      (model, format response)

    FetchFail error ->
      ({model | response = toString error}, Cmd.none)

    Select id ->
      let
        updateSelected t =
          if t.id == id then
            { t | selected = not t.selected}
          else
            t
      in
      ({model | properties = List.map updateSelected model.properties}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "url", onInput Url ] []
    , button [ onClick GetData ] [ text "Get Data"]
    , section [] [ viewEntries model.properties ]
    ]


viewEntries : List Property -> Html Msg
viewEntries properties =
        section
            [ class "main" ]
            [ Keyed.ul [] <|
                List.map viewKeyedEntry properties
            ]

viewKeyedEntry : Property -> ( String, Html Msg )
viewKeyedEntry property =
    ( toString property.id, lazy viewEntry property )

viewEntry : Property -> Html Msg
viewEntry property =
    -- p [ classList [ ("selected", property.selected)
    --               , ("unselected", property.selected == False)
    --               ]
    --   , onClick (Select property.id)
    --   ]
    section [] [viewJson property.value]

-- SUBSCRIPTIONS
port javascriptValues : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  javascriptValues Add



-- HTTP


getJson : String -> Cmd Msg
getJson url =
  Task.perform FetchFail FetchSucceed (Http.getString ("http://localhost:4000/response?url=" ++ url))

-- PARSER

quote = "\""
indentChars = "{["
outdentChars = "}]"
newLineChars = ","
uniqueHead = "##FORMAT##"
incr = 20

testString = "[{\"id\":91541985,\"time\":\"2016-10-29 01:48:04 UTC\",\"anon_visitor_id\":\"a86adf6b-910b-2b08-e291-c682\",\"ip_address\":\"76.20.48.125\",\"identity\":null,\"page\":\"https://trueme.goodhire.com/member/report-shared?candidateid=4402330f-4636-4323-a049-5a43643e69f9\",\"referrer\":null,\"user_agent\":\"Mozilla/5.0 (iPad; CPU OS 9_3_4 like Mac OS X) AppleWebKit/601.1.46 (KHTML, like Gecko) Mobile/13G35\",\"nudge_id\":167540,\"nudge_name\":\"Candidate Satisfaction\",\"answered_questions\":{\"321141\":{\"question_id\":321141,\"question_title\":\"How satisfied are you with your experience with GoodHire?\",\"question_type\":\"radio\",\"answer\":\"Very Satisfied\",\"selected_option_id\":919755}}}]"

viewJson : String -> Html msg
viewJson json =
  let
    lines =
      json
      |> formatString "" False 0
      |> String.split uniqueHead
  in
  pre [] <| List.map viewLine lines

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

px : Int -> String
px int =
  toString int
  ++ "px"

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
