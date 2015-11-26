module Cell where

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import String

type CellStatus = EXPLODED | FLAGGED | REVEALED | NORMAL

type alias Cell = {
  x: Int,
  y: Int,
  mine: Bool,
  status: CellStatus
}

view : Cell -> Html
view cell =
  div [style [
      ("background-color", "#bdbdbd"),
      ("border-color", "#7b7b7b"),
      ("border-top-color", "#fff"),
      ("border-left-color", "#fff"),
      ("border-style", "solid"),
      ("border-width", "2"),
      ("box-sizing", "border-box"),
      ("color", (getColor cell)),
      ("display", "inline-block"),
      ("line-height", "16px"),
      ("overflow", "hidden"),
      ("text-align", "center"),
      ("height", "16px"),
      ("width", "16px")
    ]]
  [text (getStatusText cell)]

getColor : Cell -> String
getColor cell =
  case (getAdjacentMineCount cell) of
    1 -> "blue"
    2 -> "green"
    3 -> "red"
    4 -> "navy"
    5 -> "#8d0000"
    6 -> "#008380"
    7 -> "black"
    8 -> "#808080"
    otherwise -> "transparent"

type alias AdjacentMineCount = Int

getStatusText : Cell -> String
getStatusText cell =
  case cell.status of
    REVEALED -> if cell.mine then "*" else toString (getAdjacentMineCount cell)
    EXPLODED -> "*"
    FLAGGED  -> "!"
    NORMAL   -> ""

getAdjacentMineCount : Cell -> AdjacentMineCount
getAdjacentMineCount cell =
  2
