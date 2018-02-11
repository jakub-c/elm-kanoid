module Main exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    List Int


init =
    ( [ 10, 20, 30, 40, 50, 60, 70 ], Cmd.none )



-- UPDATE


type Msg
    = MoveUp


update : Msg -> Model -> ( List Int, Cmd Msg )
update msg model =
    case msg of
        MoveUp ->
            ( List.map (\el -> el + 10) model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        squares =
            List.map (\el -> drawSqr (toFloat (el * 2)) 10 (el * 2)) model
    in
    div [ onClick MoveUp ]
        [ toHtml (collage 1000 500 squares) ]


drawSqr : Float -> Float -> Int -> Form
drawSqr movX movY rCol =
    move ( movX, movY )
        (filled
            (rgb rCol 100 100)
            (rect 10 10)
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
