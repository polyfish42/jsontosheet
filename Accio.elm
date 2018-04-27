port module Accio exposing (..)

import Animation exposing (px)
import Array
import Debug
import Dialog
import Dict
import GoogleSheet
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Maybe exposing (..)
import Navigation
import OAuth
import Platform.Cmd exposing (..)
import Process exposing (..)
import Regex exposing (..)
import String
import Task exposing (..)
import Time exposing (..)
import UrlParser exposing (parseHash)


main =
    Navigation.program
        (always NoOp)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { input : Maybe Input
    , errorMessage : String
    , token : Maybe String
    , spreadsheetUrl : String
    , showDialog : Bool
    , style : Animation.State
    }


type Input
    = ApiUrl String
    | Json String


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model (OAuth.parseState location |> decodeState) "" Nothing "" False (Animation.style [ Animation.opacity 1.0 ]), Cmd.batch [ Navigation.modifyUrl "#", OAuth.parseToken location |> saveToken ] )


decodeState : Maybe String -> Maybe Input
decodeState state =
    case state of
        Just string ->
            case decodeUri string of
                Just str ->
                    if contains (regex "{") str then
                        Just (Json str)
                    else
                        Just (ApiUrl str)

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


saveToken : Maybe String -> Cmd Msg
saveToken location =
    case location of
        Just str ->
            setAndGetToken (Just str)

        Nothing ->
            getToken Nothing



-- UPDATE


type Msg
    = NoOp
    | Input String
    | Animate Animation.Msg
    | Authorize
    | OpenDialog
    | CloseDialog
    | Convert
    | FetchJson (Result Http.Error String)
    | CreateSheet (Result Http.Error String)
    | TokenValue (Maybe String)
    | ValidateToken (Result Http.Error String)


port setAndGetToken : Maybe String -> Cmd msg


port getToken : Maybe String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Animate animMsg ->
            ( { model
                | style = Animation.update animMsg model.style
              }
            , Cmd.none
            )

        Input str ->
            ( { model | input = encodeInput str }, Cmd.none )

        OpenDialog ->
            ( { model | showDialog = True }, Cmd.none )

        CloseDialog ->
            ( { model | showDialog = False }, Cmd.none )

        Authorize ->
            ( model, Navigation.load <| OAuth.requestToken <| packageState model.input )

        Convert ->
            ( { model
                | errorMessage = ""
                , style =
                    Animation.interrupt
                        [ Animation.loop
                            [ Animation.to
                                [ Animation.opacity 0
                                ]
                            , Animation.to
                                [ Animation.opacity 1
                                ]
                            ]
                        ]
                        model.style
              }
            , convert model.input model
            )

        FetchJson (Ok response) ->
            ( model, requestCsv model.token model (GoogleSheet.createSheet response) )

        FetchJson (Err message) ->
            ( { model
                | errorMessage = parseError message
                , style =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 1
                            ]
                        ]
                        model.style
              }
            , Cmd.none
            )

        CreateSheet (Ok response) ->
            ( { model | spreadsheetUrl = response }, Cmd.none )

        CreateSheet (Err message) ->
            ( { model
                | errorMessage = parseError message
                , style =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 1
                            ]
                        ]
                        model.style
              }
            , Cmd.none
            )

        TokenValue token ->
            ( { model | token = token }, validateToken token )

        ValidateToken (Ok response) ->
            ( model, setExpiration response )

        ValidateToken (Err _) ->
            ( model, setAndGetToken Nothing )


encodeInput : String -> Maybe Input
encodeInput str =
    if contains (regex "{") str then
        Just (Json str)
    else
        Just (ApiUrl str)


convert : Maybe Input -> Model -> Cmd Msg
convert input model =
    case input of
        Just (Json str) ->
            requestCsv model.token model (GoogleSheet.createSheet str)

        Just (ApiUrl str) ->
            getJson str

        Nothing ->
            Cmd.none


parseError : Http.Error -> String
parseError error =
    case error of
        BadStatus res ->
            String.concat [ toString res.status.code, " Error: ", res.status.message, "\n(Try entering the url in your browser, and pasting the json here.)" ]

        _ ->
            "There was an error trying to fetch the Json. Try entering the url in your browser, and pasting the json here."



-- BadStatus { status = { code = 404, message = "" }, headers = Dict.fromList [("cache-control","no-cache"),("content-type","application/json; charset=utf-8"),("expires","-1"),("pragma","no-cache")], url = "https://jsonplaceholder.typicode.com/comme", body = "{}" }


validateToken : Maybe String -> Cmd Msg
validateToken token =
    case token of
        Just token ->
            Http.send ValidateToken <|
                Http.getString (validateTokenUrl token)

        Nothing ->
            Cmd.none


validateTokenUrl : String -> String
validateTokenUrl token =
    OAuth.url
        "https://www.googleapis.com/oauth2/v3/tokeninfo"
        [ ( "access_token", token ) ]


setExpiration : String -> Cmd Msg
setExpiration response =
    case D.decodeString (D.maybe (D.field "expires_in" D.string)) response of
        Ok (Just expiration) ->
            delay (Time.second * Result.withDefault 0 (String.toFloat expiration)) <| TokenValue Nothing

        Ok Nothing ->
            setAndGetToken Nothing

        Err _ ->
            setAndGetToken Nothing


delay : Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity



-- SUBSCRIPTIONS


port getTokenResponse : (Maybe String -> msg) -> Sub msg


port setAndGetTokenResponse : (Maybe String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ setAndGetTokenResponse TokenValue
        , getTokenResponse TokenValue
        , Animation.subscription Animate [ model.style ]
        ]



-- VIEW


view : Model -> Html Msg
view model =
    body
        [ class "container-fluid" ]
        [ bootstrap
        , div [ class "row" ]
            [ div [ class "col-md-6" ]
                [ h1 [] [ text "Turn JSON into a Google Sheet" ]
                , errorAlert model
                , inputOrLink model
                , Dialog.view
                    (if model.showDialog then
                        Just (dialogConfig model)
                     else
                        Nothing
                    )
                , footer [ style [ ( "margin-top", "80px" ) ] ] [ text "For feedback, please ", a [ href "https://github.com/polyfish42/accio/issues" ] [ text "open an issue on Github. " ], text "Created by ", a [ href "https://twitter.com/polyfish42" ] [ text " @polyfish42" ], text " ", a [ href "/privacy.html" ] [ text "Privacy Policy" ] ]
                ]
            ]
        ]


bootstrap : Html Msg
bootstrap =
    node "link"
        [ href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
        , rel "stylesheet"
        ]
        []


errorAlert : Model -> Html Msg
errorAlert model =
    case model.errorMessage of
        "" ->
            div [] []

        _ ->
            div [ class "alert alert-danger" ] [ text model.errorMessage ]


inputOrLink : Model -> Html Msg
inputOrLink model =
    case model.spreadsheetUrl of
        "" ->
            div []
                [ textarea [ placeholder "Enter your JSON or URL here.", class "form-control", rows 10, cols 60, onInput Input ] [ showInput model.input ]
                , authorizeOrConvert model
                ]

        url ->
            div [ style [ ( "height", "215px" ), ( "text-align", "center" ), ( "max-width", "518px" ) ] ]
                [ a [ href url, class "btn btn-primary", style [ ( "margin-top", "100px" ) ] ] [ text "Click here to see your spreadsheet" ]
                ]


showInput : Maybe Input -> Html Msg
showInput input =
    case input of
        Just (Json str) ->
            text str

        Just (ApiUrl str) ->
            text str

        Nothing ->
            text ""


authorizeOrConvert : Model -> Html Msg
authorizeOrConvert model =
    case Debug.log "view model" model.token of
        Just str ->
            div []
                [ button
                    (Animation.render model.style
                        ++ [ onClick Convert
                           , style
                                [ ( "margin-top", "10px" )
                                , ( "float", "right" )
                                ]
                           , class "btn btn-primary"
                           ]
                    )
                    [ text "Convert" ]
                ]

        Nothing ->
            div []
                [ button [ class "btn btn-default", onClick OpenDialog, style [ ( "margin-top", "10px" ), ( "float", "right" ) ] ] [ text "Convert" ]
                , button [ class "btn btn-primary", onClick Authorize, style [ ( "margin", "10px 10px 0 0 " ), ( "float", "right" ) ] ] [ text "Connect to Google" ]
                ]


packageState : Maybe Input -> String
packageState url =
    case url of
        Just (Json str) ->
            encodeUri str

        Just (ApiUrl str) ->
            encodeUri str

        Nothing ->
            ""


dialogConfig : Model -> Dialog.Config Msg
dialogConfig model =
    { closeMessage = Just CloseDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Connect to Google" ])
    , body = Just (p [] [ text "Before converting to a Google Sheet, you must first connect your Google Account" ])
    , footer = Just (button [ class "btn btn-primary", onClick Authorize ] [ text "Authorize" ])
    }



-- HTTP


getJson : String -> Cmd Msg
getJson url =
    Http.send FetchJson <|
        Http.getString url


requestCsv : Maybe String -> Model -> E.Value -> Cmd Msg
requestCsv token model requestBody =
    case token of
        Just token ->
            Http.send CreateSheet (putRequest token model requestBody)

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
