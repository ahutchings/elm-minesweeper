module Row where

import Cell
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Row = List Cell.Cell

view : Row -> Html
view row =
  div [] (List.map Cell.view row)
