module GoogleSheet exposing (..)

import Array
import Debug
import Dict
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
import String

type JsonVal
    = JsonString String
    | JsonObject (Dict.Dict String JsonVal)
    | JsonFloat Float
    | JsonInt Int
    | JsonNull
    | JsonArray (List JsonVal)
    
createSheet : String -> E.Value
createSheet response =
    D.decodeString jsonDecoder response
        |> flattenAndEncode
        |> List.map createRow
        |> googleSheetsRequestBody


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


createRow : List ( String, E.Value ) -> E.Value
createRow row =
    E.object
        [ ( "values"
          , E.array
                (Array.fromList
                    (List.map cells row)
                )
          )
        ]


cells cell =
    case cell of
        ( key, str ) ->
            E.object
                [ ( "userEnteredValue"
                  , E.object
                        [ ( "stringValue", str )
                        ]
                  )
                ]



--
-- (key, E.int int) ->
--   E.object
--       [ ( "userEnteredValue"
--         , E.object
--               [ ( "numberValue", E.int int )
--               ]
--         )
--       ]
--
-- (key, E.float float) ->
--   E.object
--       [ ( "userEnteredValue"
--         , E.object
--               [ ( "numberValue", E.float float )
--               ]
--         )
--       ]


googleSheetsRequestBody : List E.Value -> E.Value
googleSheetsRequestBody rows =
    E.object
        [ ( "sheets"
          , E.array
                (Array.fromList
                    [ E.object
                        [ ( "data"
                          , E.array
                                (Array.fromList
                                    [ E.object
                                        [ ( "rowData"
                                          , E.array
                                                (Array.fromList
                                                    rows
                                                )
                                          )
                                        ]
                                    ]
                                )
                          )
                        ]
                    ]
                )
          )
        ]
