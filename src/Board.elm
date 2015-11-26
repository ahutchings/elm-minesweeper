module Board where

import Html exposing (..)
import Html.Attributes exposing (..)
import Row

type alias Board = List Row.Row

view : Board -> Html
view board =
  div [] (List.map Row.view board)
