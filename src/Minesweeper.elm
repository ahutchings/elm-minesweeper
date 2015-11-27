module Minesweeper where

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Random exposing (initialSeed, Seed)
import Signal exposing (Signal, Address)
import String
import Time exposing (Time)
import Window

type GameStatus = WON | LOST | ACTIVE

type alias Model = {
  board: Board,
  elapsedTime: Time
}

getGameStatus : Model -> GameStatus
getGameStatus game =
  WON

generateBoard : Int -> Int -> Int -> Seed -> Board
generateBoard rows columns mines seed =
  placeMines (generateEmptyBoard rows columns) mines seed

generateEmptyBoard : Int -> Int -> Board
generateEmptyBoard rows columns =
  let generateRowWithColumns = generateRow columns
  in
    List.map generateRowWithColumns (Array.toList (Array.initialize rows identity))

generateRow : Int -> Int -> Row
generateRow columns y =
  let generateCellWithY = generateCell y
  in
    List.map generateCellWithY (Array.toList (Array.initialize columns identity))

generateCell : Int -> Int -> Cell
generateCell y x =
  {
    x = x,
    y = y,
    mine = False,
    status = NORMAL
  }

placeMines : Board -> Int -> Seed -> Board
placeMines board mines seed =
  board

elapsedTime : Model -> Html
elapsedTime game =
  div [] [text "1"]

resetButton : Model -> Html
resetButton game =
  let buttonText =
    case getGameStatus game of
      WON -> "B-)"
      LOST -> ":("
      ACTIVE -> ":)"
  in
    button [class "reset"] [text buttonText]

remainingFlags : Model -> Html
remainingFlags model =
  let count = getRemainingFlags model.board
  in
    div [class "remaining-flags"] [text (toString count)]

getRemainingFlags : Board -> Int
getRemainingFlags board =
  1

type Action
  = NoOp
  | RevealCell Cell
  | FlagCell Cell

type CellStatus = EXPLODED | FLAGGED | REVEALED | NORMAL

type alias Cell = {
  x: Int,
  y: Int,
  mine: Bool,
  status: CellStatus
}

cellView : Address Action -> Cell -> Html
cellView address cell =
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
      ],
      onClick address (RevealCell cell),
      onContextMenu address (FlagCell cell)
    ]
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

type alias Row = List Cell

rowView : Address Action -> Row -> Html
rowView address row =
  div [] (List.map (cellView address) row)

type alias Board = List Row

boardView : Address Action -> Board -> Html
boardView address board =
  div [] (List.map (rowView address) board)


view : Address Action -> Model -> Html
view address model =
  div [class "board"] [
    (resetButton model),
    (elapsedTime model),
    (remainingFlags model),
    (boardView address model.board)
  ]

main : Signal Html
main =
  Signal.map (view actions.address) model

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal

initialModel : Model
initialModel = {
    board = generateBoard 8 8 10 (initialSeed 31415),
    elapsedTime = 0
  }


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    RevealCell cell -> model
    FlagCell cell -> model
