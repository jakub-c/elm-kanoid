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
      , height = 100
      , width = 400
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


view : { a | x : Int, y : Int } -> Html msg
view model =
    div []
        [ toHtml (collage 1000 500 [ drawSqr model.x model.y 10 ]) ]


drawSqr : Int -> Int -> Int -> Form
drawSqr movX movY rCol =
    move ( toFloat movX, toFloat movY )
        (filled
            (rgb rCol 100 100)
            (rect 10 10)
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.presses KeyMsg ]
