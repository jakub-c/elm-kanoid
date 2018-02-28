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
    { w : Int, h : Int }


type alias KeyPressed =
    { left : Bool
    , right : Bool
    }


type alias Player =
    { x : Int
    , y : Int
    , height : Int
    , width : Int
    , velocity : Int
    , window : Window
    , keyPressed : KeyPressed
    }


type alias Model =
    Player


init =
    ( { x = 0
      , y = round (-480 / 2) + 10
      , height = 20
      , width = 80
      , velocity = 5
      , window =
            { w = 640
            , h = 480
            }
      , keyPressed =
            { left = False
            , right = False
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | Tick Time


updateLeftKey : KeyPressed -> Bool -> KeyPressed
updateLeftKey data val =
    { data | left = val }


updateRightKey : KeyPressed -> Bool -> KeyPressed
updateRightKey data val =
    { data | right = val }


update msg model =
    case msg of
        KeyDown code ->
            if code == 37 then
                ( { model | keyPressed = updateLeftKey model.keyPressed True }, Cmd.none )
            else if code == 39 then
                ( { model | keyPressed = updateRightKey model.keyPressed True }, Cmd.none )
            else
                ( model, Cmd.none )

        KeyUp code ->
            if code == 37 then
                ( { model | keyPressed = updateLeftKey model.keyPressed False }, Cmd.none )
            else if code == 39 then
                ( { model | keyPressed = updateRightKey model.keyPressed False }, Cmd.none )
            else
                ( model, Cmd.none )

        Tick newTime ->
            let
                leftWallX =
                    round ((toFloat model.window.w / 2) * (-1) + (toFloat model.width / 2))

                rightWallX =
                    round ((toFloat model.window.w / 2) - (toFloat model.width / 2))
            in
                if (model.keyPressed.left == True && model.x >= leftWallX) then
                    ( { model | x = model.x - model.velocity }, Cmd.none )
                else if (model.keyPressed.right == True && model.x <= rightWallX) then
                    ( { model | x = model.x + model.velocity }, Cmd.none )
                else
                    ( model, Cmd.none )


view model =
    div []
        [ toHtml (collage model.window.w model.window.h [ drawSqr model.x model.y model.height model.width ]) ]


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
