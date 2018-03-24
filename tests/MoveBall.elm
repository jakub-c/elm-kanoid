module MoveBall exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)


mockBallMovTopLeft =
    { player =
        { x = -100
        , y = -100
        , height = 10
        , width = 10
        , velocity = 0
        }
    , ball =
        { x = 0
        , y = 0
        , w = 20
        , h = 20
        , init = False
        , velX = -5
        , velY = -5
        }
    }


mockBallMovTopLeftWall =
    { window =
        { w = 640
        }
    , player =
        { x = -100
        , y = -100
        , height = 10
        , width = 10
        , velocity = 0
        }
    , ball =
        { x = -319
        , y = 0
        , w = 20
        , h = 20
        , init = False
        , velX = -5
        , velY = -5
        }
    }


mockBallMovTopRightWall =
    { window =
        { w = 640
        }
    , player =
        { x = -100
        , y = -100
        , height = 10
        , width = 10
        , velocity = 0
        }
    , ball =
        { x = 295
        , y = 116
        , w = 20
        , h = 20
        , init = False
        , velX = 5
        , velY = 5
        }
    }


suite : Test
suite =
    describe "ball movements"
        [ describe "ball moves no collision"
            [ test
                "function: moveBall"
              <|
                \_ ->
                    let
                        newModel =
                            moveBall mockBallMovTopLeft
                    in
                        Expect.equal newModel.ball.x -5
            ]
        , describe "function: ballCollisionWallX"
            [ test
                "ball moves top right x"
              <|
                \_ ->
                    let
                        newModel =
                            ballCollisionWallX mockBallMovTopRightWall
                    in
                        Expect.equal newModel.ball.x 300
            , test
                "ball moves top right vel"
              <|
                \_ ->
                    let
                        newModel =
                            ballCollisionWallX mockBallMovTopRightWall
                    in
                        Expect.equal newModel.ball.velX -5
            , test
                "ball moves top left"
              <|
                \_ ->
                    let
                        newModel =
                            ballCollisionWallX mockBallMovTopLeftWall
                    in
                        Expect.equal newModel.ball.x -320
            ]
        ]
