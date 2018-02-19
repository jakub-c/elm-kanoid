module Main exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Keyboard


main : Program Never { height : Int, width : Int, x : Int, y : Int } Msg
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
    }


type alias Model =
    Player


init : ( { height : Int, width : Int, x : Int, y : Int }, Cmd msg )
init =
    ( { x = 0
      , y = 0
      , height = 100
      , width = 400
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyMsg Keyboard.KeyCode


update : Msg -> { a | x : number } -> ( { a | x : number }, Cmd msg )
update msg model =
    case msg of
        KeyMsg code ->
            if code == 39 then
                ( { model | x = model.x + 1 }, Cmd.none )
            else if code == 37 then
                ( { model | x = model.x - 1 }, Cmd.none )
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
