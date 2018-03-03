module Main exposing (..)

-- import Html.Attributes exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
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
            , width = 140
            , velocity = 10
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
            , init = True
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

        updateSpaceKey data =
            { data | init = False }
    in
        case msg of
            KeyDown code ->
                if code == 37 then
                    ( { model | keyPressed = updateLeftKey model.keyPressed True }, Cmd.none )
                else if code == 39 then
                    ( { model | keyPressed = updateRightKey model.keyPressed True }, Cmd.none )
                else if code == 32 then
                    ( { model | ball = updateSpaceKey model.ball }, Cmd.none )
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
                    |> playerCollision
                    |> movePlayer model.keyPressed
                    |> moveBall
                , Cmd.none
                )


movePlayer keys model =
    let
        leftWallX =
            round ((toFloat model.window.w / 2) * (-1))

        rightWallX =
            round (toFloat model.window.w / 2) - model.player.width

        updatePlayerX data val =
            { data | x = val }
    in
        if (model.keyPressed.left == True && (model.player.x - model.player.velocity) >= leftWallX) then
            { model | player = updatePlayerX model.player (model.player.x - model.player.velocity) }
        else if (model.keyPressed.right == True && (model.player.x + model.player.velocity) <= rightWallX) then
            { model | player = updatePlayerX model.player (model.player.x + model.player.velocity) }
        else
            model


ballCollisionWallX model =
    let
        updateDirX data dirVal =
            { data | dirX = dirVal }

        rightWall =
            round (toFloat model.window.w / 2) - model.ball.w

        leftWall =
            round (toFloat model.window.w / 2) * (-1)
    in
        if
            ((model.ball.x + model.ball.velocity <= leftWall)
                || (model.ball.x - model.ball.velocity >= rightWall)
            )
        then
            { model | ball = updateDirX model.ball (model.ball.dirX * (-1)) }
        else
            model


ballCollisionWallY model =
    let
        updateDirY data dirVal =
            { data | dirY = dirVal }

        topWall =
            (round (toFloat model.window.h / 2) - round (toFloat model.ball.h))

        bottomWall =
            ((round (toFloat model.window.h / 2)) * (-1) - round (toFloat model.ball.h / 2))

        resetBall data =
            { data | init = True }
    in
        if (model.ball.y - model.ball.velocity >= topWall) then
            { model | ball = updateDirY model.ball (model.ball.dirY * (-1)) }
        else if (model.ball.y - model.ball.velocity <= bottomWall) then
            { model | ball = resetBall model.ball }
        else
            model


playerCollision model =
    let
        updateDirY data dirVal =
            { data | dirY = dirVal }

        ballX =
            model.ball.x

        playerLeftSide =
            model.player.x

        playerRightSide =
            (model.player.x + model.player.width)

        playerTopSide =
            model.player.y

        playerBottomSide =
            model.player.y + model.player.height
    in
        if
            ((model.ball.x <= model.player.x + model.player.width)
                && (model.ball.x + model.ball.w >= model.player.x)
                && (model.ball.y <= model.player.y + model.player.height)
                && (model.ball.h + model.ball.y >= model.player.y)
            )
        then
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
                        (model.player.x + round (toFloat model.player.width / 2) - round (toFloat model.ball.w / 2))
                        (model.player.y + model.player.height + 1)
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
    div
        [ style
            [ ( "border", "solid 1px red" )
            , ( "height", toString model.window.h ++ "px" )
            , ( "width", toString model.window.w ++ "px" )
            ]
        ]
        [ toHtml
            (collage model.window.w
                model.window.h
                [ drawSqr model.player.x model.player.y model.player.width model.player.height
                , drawSqr model.ball.x model.ball.y model.ball.w model.ball.h
                ]
            )
        ]


drawSqr : Int -> Int -> Int -> Int -> Form
drawSqr posX posY width height =
    move ( toFloat (posX + round (toFloat width / 2)), toFloat (posY + round (toFloat height / 2)) )
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
