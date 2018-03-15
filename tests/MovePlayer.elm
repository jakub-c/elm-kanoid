module MovePlayer exposing (..)

import Expect exposing (Expectation)


-- import Fuzz exposing (Fuzzer, int, list, string)

import Test exposing (..)
import Main exposing (..)


-- sprawdzic czy jak jest 315 i vel 10 to czy do sciany dobijasz


mockMoveLeft =
    { player =
        { x = 0
        , width = 140
        , velocity = 10
        }
    , window =
        { w = 640
        }
    , keyPressed =
        { left = True
        , right = False
        }
    }


mockMoveRight =
    { player =
        { x = 0
        , width = 140
        , velocity = 10
        }
    , window =
        { w = 640
        }
    , keyPressed =
        { left = False
        , right = True
        }
    }


mockMoveLeftTouchWall =
    { player =
        { x = -320
        , width = 140
        , velocity = 10
        }
    , window =
        { w = 640
        }
    , keyPressed =
        { left = True
        , right = False
        }
    }


mockMoveLeftPastWall =
    { player =
        { x = -315
        , width = 140
        , velocity = 10
        }
    , window =
        { w = 640
        }
    , keyPressed =
        { left = True
        , right = False
        }
    }


suite : Test
suite =
    describe "player movements"
        [ describe "test player movements"
            [ test
                "player moves left"
              <|
                \() ->
                    let
                        newModel =
                            movePlayer mockMoveLeft
                    in
                        Expect.equal newModel.player.x -10
            , test
                "player moves right"
              <|
                \() ->
                    let
                        newModel =
                            movePlayer mockMoveRight
                    in
                        Expect.equal newModel.player.x 10
            ]
        , describe "player collisions"
            [ test
                "player moves left, but is closer to the wall than the velocity distance"
              <|
                \() ->
                    let
                        newModel =
                            movePlayer mockMoveLeftPastWall
                    in
                        Expect.equal newModel.player.x -320
            ]
        ]
