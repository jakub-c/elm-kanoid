module Main exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Keyboard


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Player =
    { x : Int
    , y : Int
    , height : Int
    , width : Int
    , velocity : Int
    }


type alias Model =
    Player


init =
    ( { x = 0
      , y = 0
      , height = 20
      , width = 80
      , velocity = 5
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyMsg Keyboard.KeyCode


update msg model =
    case msg of
        KeyMsg code ->
            if code == 39 then
                ( { model | x = model.x + model.velocity }, Cmd.none )
            else if code == 37 then
                ( { model | x = model.x - model.velocity }, Cmd.none )
            else
                ( model, Cmd.none )


view model =
    div []
        [ toHtml (collage 1000 500 [ drawSqr model.x model.y model.height model.width ]) ]


drawSqr : Int -> Int -> Int -> Int -> Form
drawSqr posX posY height width =
    move ( toFloat posX, toFloat posY )
        (filled
            (rgb 100 100 100)
            (rect (toFloat width) (toFloat height))
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.presses KeyMsg ]
