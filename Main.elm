module Main exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)


main : Html.Html msg
main =
    toHtml (collage 100 100 [ filled (rgb 100 100 100) (rect 10 10) ])
