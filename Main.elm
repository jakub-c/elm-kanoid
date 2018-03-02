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


type alias Ball =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    , velocity : Int
    , init : Bool
    , dirX : Int
    , dirY : Int
    }


type alias Player =
    { x : Int
    , y : Int
    , height : Int
    , width : Int
    , velocity : Int
    }


type alias Game =
    { player : Player
    , window : Window
    , keyPressed : KeyPressed
    , ball : Ball
    }


type alias Model =
    Game


init =
    ( { player =
            { x = 0
            , y = round (-480 / 2) + 10
            , height = 20
            , width = 80
            , velocity = 5
            }
      , window =
            { w = 640
            , h = 480
            }
      , keyPressed =
            { left = False
            , right = False
            }
      , ball =
            { x = 0
            , y = 0
            , w = 20
            , h = 20
            , velocity = 5
            , init = False
            , dirX = 1
            , dirY = -1
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | Tick Time


update msg model =
    let
        updateLeftKey : KeyPressed -> Bool -> KeyPressed
        updateLeftKey data val =
            { data | left = val }

        updateRightKey : KeyPressed -> Bool -> KeyPressed
        updateRightKey data val =
            { data | right = val }
    in
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
                ( model
                    |> ballCollisionWallX
                    |> ballCollisionWallY
                    |> movePlayer model.keyPressed
                    |> moveBall
                , Cmd.none
                )


movePlayer keys model =
    let
        leftWallX =
            round ((toFloat model.window.w / 2) * (-1) + (toFloat model.player.width / 2))

        rightWallX =
            round ((toFloat model.window.w / 2) - (toFloat model.player.width / 2))

        updatePlayerX data val =
            { data | x = val }
    in
        if (model.keyPressed.left == True && model.player.x >= leftWallX) then
            { model | player = updatePlayerX model.player (model.player.x - model.player.velocity) }
        else if (model.keyPressed.right == True && model.player.x <= rightWallX) then
            { model | player = updatePlayerX model.player (model.player.x + model.player.velocity) }
        else
            model


ballCollisionWallX model =
    let
        updateDirX data dirVal =
            { data | dirX = dirVal }

        rightWall =
            (round (toFloat model.window.w / 2) - round (toFloat model.ball.w / 2))

        leftWall =
            ((round (toFloat model.window.w / 2)) * (-1) + round (toFloat model.ball.w / 2))
    in
        if (model.ball.x >= rightWall || model.ball.x <= leftWall) then
            { model | ball = updateDirX model.ball (model.ball.dirX * (-1)) }
        else
            model


ballCollisionWallY model =
    let
        updateDirY data dirVal =
            { data | dirY = dirVal }

        bottomWall =
            (round (toFloat model.window.h / 2) - round (toFloat model.ball.h / 2))

        topWall =
            ((round (toFloat model.window.h / 2)) * (-1) + round (toFloat model.ball.h / 2))
    in
        if (model.ball.y >= bottomWall || model.ball.y <= topWall) then
            { model | ball = updateDirY model.ball (model.ball.dirY * (-1)) }
        else
            model


moveBall model =
    let
        updateBallPos data valX valY =
            { data | x = valX, y = valY }
    in
        if (model.ball.init == True) then
            { model
                | ball =
                    updateBallPos
                        model.ball
                        model.player.x
                        (model.player.y + model.player.height)
            }
        else
            { model
                | ball =
                    updateBallPos
                        model.ball
                        (model.ball.x + model.ball.dirX * model.ball.velocity)
                        (model.ball.y - model.ball.dirY * model.ball.velocity)
            }


view model =
    div []
        [ toHtml
            (collage model.window.w
                model.window.h
                [ drawSqr model.player.x model.player.y model.player.height model.player.width
                , drawSqr model.ball.x model.ball.y model.ball.h model.ball.w
                ]
            )
        ]


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
