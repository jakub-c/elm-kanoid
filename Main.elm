module Main exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)


main : Html.Html msg
main =
    let
        squares =
            List.map (\el -> drawSqr (toFloat el * 20) 10 (el * 40)) (List.range 1 10)
    in
    toHtml (collage 1000 500 squares)


drawSqr : Float -> Float -> Int -> Form
drawSqr movX movY rCol =
    move ( movX, movY )
        (filled
            (rgb rCol 100 100)
            (rect 10 10)
        )
