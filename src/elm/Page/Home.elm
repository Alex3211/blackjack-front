module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Asset
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , counter : Int
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Let's play"
      , pageBody = "This is the home page"
      , counter = 0
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
            , h5 [] [ text "Counter" ]
            , p []
                [ text (String.fromInt model.counter)
                ]
            , button [ onClick IncreaseCounter ] [ text "+" ]
            , button [ onClick DecreaseCounter ] [ text "-" ]
            ]
    }



-- UPDATE


type Msg
    = IncreaseCounter
    | DecreaseCounter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncreaseCounter ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        DecreaseCounter ->
            ( { model | counter = model.counter - 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
