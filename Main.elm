module Main exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, update = update, view = view }



-- MODEL


type alias Model =
    Int


model : Model
model =
    20



-- UPDATE


type Msg
    = MoveUp


update : Msg -> Model -> number
update msg model =
    case msg of
        MoveUp ->
            10



-- VIEW


view : Model -> Html Msg
view model =
    let
        squares =
            List.map (\el -> drawSqr (toFloat el * toFloat model) 10 (el * 40)) (List.range 1 10)
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
