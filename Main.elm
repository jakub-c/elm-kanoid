module Main exposing (..)

-- import Html.Attributes exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Keyboard
import Time exposing (Time)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TODO implement game loop:
-- https://stackoverflow.com/questions/12273451/how-to-fix-delay-in-javascript-keydown
-- MODEL


type alias Window =
    { h : Int, w : Int }


type alias Player =
    { x : Int
    , y : Int
    , height : Int
    , width : Int
    , velocity : Int
    , window : Window
    , keyPressed : Int
    }


type alias Model =
    Player


init =
    ( { x = 0
      , y = round (-480 / 2) + 10
      , height = 20
      , width = 80
      , velocity = 5
      , window = { h = 640, w = 480 }
      , keyPressed = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | Tick Time


update msg model =
    case msg of
        KeyDown code ->
            ( { model | keyPressed = code }, Cmd.none )

        KeyUp code ->
            ( model, Cmd.none )

        Tick newTime ->
            if model.keyPressed == 39 then
                ( { model | x = model.x + model.velocity }, Cmd.none )
            else if model.keyPressed == 37 then
                ( { model | x = model.x - model.velocity }, Cmd.none )
            else
                ( model, Cmd.none )


view model =
    div []
        [ toHtml (collage model.window.h model.window.w [ drawSqr model.x model.y model.height model.width ]) ]


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
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , times Tick
        ]
