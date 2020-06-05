module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Dict
import Asset
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Session exposing (..)
import Result exposing (Result)
import Page.About exposing (Model)



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
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Let's play"
      , pageBody = "This is the home page"
      , content = ""
      , error = ""
      , result = ""
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 [ align "center" ] [ text model.pageTitle ]
            , div [ align "center" ] [ img [ Asset.srcFromString "Blackjack.png" ] [] ]
            , hr [] []
            , p [] []
            , div [ class "form-group"]
                [ input [ class "form-control", placeholder "Pseudo", Html.Attributes.value model.content, onInput Change ] []
                , button [ class "btn btn-secondary", onClick SendPseudo  ] [ text "Set" ]
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
    | SendPseudo (Result Http.Error String)

getCards : Cmd Msg
getCards =
  Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString DataReceived
    }

sendPseudo : Cmd Msg
sendPseudo  =
  Http.post
    { url = "https://example.com/books"
    , body = pseudo
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

        
        SendPseudo(Ok value) -> ( { model | result = value }, Cmd.none )
        SendPseudo(_) -> ( { model | error = "Error during request" }, Cmd.none )

            -- ( { model | result = (decodeString (keyValuePairs int) data). }, Cmd.none )

-- decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
--   == Ok [("alice", 42), ("bob", 99)]
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
