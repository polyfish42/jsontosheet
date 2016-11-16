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
  | Add
  | UpdateField String
  | GetData
  | FetchSucceed String
  | FetchFail Http.Error
  | Display String
  | Select Int

port format : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Add ->
      ({ model
        | uid = model.uid + 1
        , field = ""
        , properties =
          if String.isEmpty model.field then
                model.properties
            else
                model.properties ++ [ newProperty model.field model.uid ]
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

    Display javascriptValue ->
      ({model | response = prettify javascriptValue}, Cmd.none)

    Select id ->
      let
        updateSelected t =
          if t.id == id then
            { t | selected = not t.selected}
          else
            t
      in
      ({model | properties = List.map updateSelected model.properties}, Cmd.none)

prettify : String -> String
prettify = Regex.replace All(Regex.regex ",") (\_ -> ",</p><p>")

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "url", onInput Url ] []
    , button [ onClick GetData ] [ text "Get Data"]
    , section []
              [ lazy viewInput model.field
              , lazy viewEntries model.properties
              ]
    , div [] [ text model.response ]
    ]

viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "Properties" ]
        , input
            [ placeholder "What property do you want to add?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onClick Add
            ]
            []
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
    p [ classList [ ("selected", property.selected)
                  , ("unselected", property.selected == False)
                  ]
      , onClick (Select property.id)
      ]
      [text property.value]

-- SUBSCRIPTIONS
port javascriptValues : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  javascriptValues Display



-- HTTP


getJson : String -> Cmd Msg
getJson url =
  Task.perform FetchFail FetchSucceed (Http.getString ("http://localhost:4000/response?url=" ++ url))
  -- Task.perform FetchFail FetchSucceed (Http.getString (url))
