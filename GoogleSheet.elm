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
    | JsonBool Bool
    | JsonArray (List JsonVal)


createSheet : String -> E.Value
createSheet response =
    D.decodeString jsonDecoder response
        |> flattenAndEncode
        |> headersAndRows
        |> googleSheetsRequestBody


jsonDecoder : Decoder JsonVal
jsonDecoder =
    D.oneOf
        [ D.map JsonString D.string
        , D.map JsonInt D.int
        , D.map JsonFloat D.float
        , D.list (D.lazy (\_ -> jsonDecoder)) |> D.map JsonArray
        , D.dict (D.lazy (\_ -> jsonDecoder)) |> D.map JsonObject
        , D.map JsonBool D.bool
        , D.null JsonNull
        ]


flattenAndEncode : Result String JsonVal -> List (List ( String, JsonVal ))
flattenAndEncode json =
    case json of
        Ok response ->
            case response of
                JsonObject obj ->
                    [ destructure [] "" (JsonObject obj) ]

                JsonArray list ->
                    List.map (destructure [] "") list

                _ ->
                    [ [ ( "error", JsonString "irregular json" ) ] ]

        Err message ->
            [ [ ( "There was an error making this sheet.", JsonString message ) ] ]


destructure : List ( String, JsonVal ) -> String -> JsonVal -> List ( String, JsonVal )
destructure acc nestedName jsonVal =
    case jsonVal of
        JsonObject object ->
            case (Dict.toList object) of
                x :: xs ->
                    case x of
                        ( key, JsonString val ) ->
                            destructure (( nestKeys nestedName key, JsonString val ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonInt val ) ->
                            destructure (( nestKeys nestedName key, JsonInt val ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonFloat val ) ->
                            destructure (( nestKeys nestedName key, JsonFloat val ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonNull ) ->
                            destructure (( nestKeys nestedName key, JsonNull ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonBool boolian ) ->
                            destructure (( nestKeys nestedName key, JsonBool boolian ) :: acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonArray list ) ->
                            destructure ((destructureArray nestedName key list [] 0) ++ acc) nestedName (JsonObject (Dict.fromList xs))

                        ( key, JsonObject obj ) ->
                            (destructure acc "" (JsonObject (Dict.fromList xs))) ++ (destructure [] (nestKeys nestedName key) (JsonObject obj))

                x ->
                    case x of
                        [ ( key, JsonString val ) ] ->
                            ( nestKeys nestedName key, JsonString val ) :: acc

                        [ ( key, JsonInt val ) ] ->
                            ( nestKeys nestedName key, JsonInt val ) :: acc

                        [ ( key, JsonFloat val ) ] ->
                            ( nestKeys nestedName key, JsonFloat val ) :: acc

                        [ ( key, JsonNull ) ] ->
                            ( nestKeys nestedName key, JsonNull ) :: acc

                        [ ( key, JsonBool boolian ) ] ->
                            ( nestKeys nestedName key, JsonBool boolian ) :: acc

                        [ ( key, JsonArray list ) ] ->
                            (destructureArray nestedName key list [] 0) ++ acc

                        [ ( key, JsonObject obj ) ] ->
                            destructure acc (nestKeys nestedName key) (JsonObject obj)

                        _ ->
                            acc

        _ ->
            [ ( "y", JsonString "case" ) ]


nestKeys : String -> String -> String
nestKeys nestedNames key =
    case nestedNames of
        "" ->
            key

        str ->
            str ++ "/" ++ key


destructureArray : String -> String -> List JsonVal -> List ( String, JsonVal ) -> Int -> List ( String, JsonVal )
destructureArray nestedName key list acc counter =
    case list of
        x :: xs ->
            case x of
                JsonString str ->
                    destructureArray nestedName key xs (( (nestKeys nestedName key) ++ "/" ++ (toString counter), JsonString str ) :: acc) (counter + 1)

                JsonInt int ->
                    destructureArray nestedName key xs (( (nestKeys nestedName key) ++ "/" ++ (toString counter), JsonInt int ) :: acc) (counter + 1)

                JsonNull ->
                    destructureArray nestedName key xs (( (nestKeys nestedName key) ++ "/" ++ (toString counter), JsonNull ) :: acc) (counter + 1)

                _ ->
                    [ ( "error", JsonNull ) ]

        [] ->
            acc


headersAndRows : List (List ( String, JsonVal )) -> (List E.Value, Int)
headersAndRows rows =
    Maybe.withDefault [ ( "There was an error with the headers in this app", JsonNull ) ] (List.head rows)
        |> createHeaders
        |> Tuple.mapFirst (List.append (List.map createRow rows))
        |> Tuple.mapFirst (List.reverse)


createHeaders : List ( String, JsonVal ) -> (List E.Value, Int)
createHeaders row =
    ([ E.object
        [ ( "values"
          , E.array
                (Array.fromList
                    (List.map firstTuple row)
                )
          )
        ]
    ]
    , List.length row)


firstTuple : ( String, JsonVal ) -> E.Value
firstTuple row =
    case row of
        ( header, value ) ->
            googleStringCell header


createRow : List ( String, JsonVal ) -> E.Value
createRow row =
    E.object
        [ ( "values"
          , E.array
                (Array.fromList
                    (List.map cells row)
                )
          )
        ]


cells : ( String, JsonVal ) -> E.Value
cells cell =
    case cell of
        ( key, JsonString str ) ->
            googleStringCell str

        ( key, JsonInt int ) ->
            googleNumberCell (toString int)

        ( key, JsonFloat float ) ->
            googleNumberCell (toString float)

        ( key, JsonBool boolian ) ->
            googleStringCell (toString boolian)

        ( key, JsonNull ) ->
            googleStringCell "null"

        _ ->
            E.object
                [ ( "userEnteredValue"
                  , E.object
                        [ ( "stringValue", E.string "There was an error parsing this cell" )
                        ]
                  )
                ]


googleStringCell : String -> E.Value
googleStringCell str =
    E.object
        [ ( "userEnteredValue"
          , E.object
                [ ( "stringValue", E.string str )
                ]
          )
        ]


googleNumberCell : String -> E.Value
googleNumberCell num =
    E.object
        [ ( "userEnteredValue"
          , E.object
                [ ( "numberValue", E.string num )
                ]
          )
        ]


googleSheetsRequestBody : (List E.Value, Int) -> E.Value
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
                                                    (Tuple.first rows)
                                                )
                                          )
                                        ]
                                    ]
                                )
                          )
                        , ( "properties"
                          , E.object
                                [ ( "gridProperties"
                                  , E.object
                                        [ ( "columnCount", E.int (columnLength rows) ) ]
                                  )
                                , ("title", E.string "From Accio")
                                ]
                          )
                        ]
                    ]
                )
          )
        ]

columnLength : (List E.Value, Int) -> Int
columnLength rows=
  if Tuple.second rows > 26 then
    Tuple.second rows
  else
    26
