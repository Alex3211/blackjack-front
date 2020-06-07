module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Asset
import Asset exposing (error)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Page.About exposing (Model)
import Result exposing (Result)
import Session exposing (..)
import String exposing (String)



type Enum cards
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Valet
    | Queen
    | King


-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , content : String
    , error : String
    , result : String
    , output : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Let's play"
      , pageBody = "This is the home page"
      , content = ""
      , error = ""
      , result = ""
      , output = ""
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    let
        outDiv = case model.error of
            Nothing ->
                div []
                    [ label [ for "outputUpcase" ] [ text "Output" ]
                    , input [ type_ "text", id "outputUpcase", readonly True, Html.Attributes.value model.output ] []
                    ]

            Just err ->
                div []
                    [ label [ for "errorUpcase" ] [ text "Error" ]
                    , input [ type_ "text", id "errorUpcase", readonly True, Html.Attributes.value err ] []
                    ]
    in
        { title = model.pageTitle
        , content =
            div [ class "container" ]
                [ h2 [ align "center" ] [ text model.pageTitle ]
                , div [ align "center" ] [ img [ Asset.srcFromString "Blackjack.png" ] [] ]
                , hr [] []
                , p [] []
                , div [ class "form-group"]
                    [ input [ class "form-control", placeholder "Pseudo", Html.Attributes.value model.content, onInput Change ] []
                    , button [ class "btn btn-secondary", onInput InputString ] [ text "Set" ]
                    , div [] [ text (String.reverse model.content) ]
                    ]
                , button [ class "btn btn-primary", onClick SendHttpRequest ] [ text "Get card" ]
                ]
        }


-- UPDATE


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error String)
    | Change String
    | UpcaseRequest ( Result Http.Error String )
    | InputString String

getCards : Cmd Msg
getCards =
  Http.get
    { url = "https://127.0.0.1:8080/game"
    , expect = Http.expectString DataReceived
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getCards ) 

        DataReceived(Ok data) -> 
            ( { model | pageBody = data }, Cmd.none)

        DataReceived(_) -> 
            ( { model | error = "Error during request" }, Cmd.none)

        Change (newContent) ->
            ( { model | content = newContent }, Cmd.none )

        UpcaseRequest (Ok response) ->
            ( { model | output = response, error = "" }, Cmd.none )

        UpcaseRequest (Err err) ->
            let
                errMsg = case err of
                    Http.Timeout ->
                        "Request timeout"

                    Http.NetworkError ->
                        "Network error"

            in
                ( { model | output = "", error = "" }, Cmd.none )

        InputString str ->
            ( model, upcaseRequest str )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


-- HELPERS

upcaseSuccessDecoder : Json.Decode.Decoder String
upcaseSuccessDecoder = Json.Decode.field "output" Json.Decode.string

upcaseErrorDecoder : Json.Decode.Decoder String
upcaseErrorDecoder = Json.Decode.field "error" Json.Decode.string

upcaseRequestEncoder : String -> Json.Encode.Value
upcaseRequestEncoder str = Json.Encode.object [ ( "input", Json.Encode.string str ) ]

upcaseRequest : String -> Cmd Msg
upcaseRequest str =
    let
        req = Http.post {
            url = "http://127.0.0.1:8080/upcase"
            , body = ( Http.jsonBody <| upcaseRequestEncoder str )
            , expect = Http.expectJson upcaseSuccessDecoder
            }
    in
        Http.post { UpcaseRequest req }