module Minesweeper where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Random
import Signal exposing (Signal, Address)
import String
import Time exposing (Time)
import Window

type CellStatus = EXPLODED | FLAGGED | REVEALED | NORMAL

type alias Cell = {
  x: Int,
  y: Int,
  mine: Bool,
  status: CellStatus
}

type alias Row = List Cell

type alias Board = {
  rows: List Row
}

type GameStatus = WON | LOST | ACTIVE

type alias Game = {
  board: Board,
  elapsedTime: Time
}

getGameStatus : Game -> GameStatus
getGameStatus game =
  WON

generateBoard : Int -> Int -> Int -> Seed -> Board
generateBoard rows columns mines seed =
  Board

resetButton : Game -> Html
resetButton game =
  let buttonText =
    case getGameStatus game of
      WON -> "B-)"
      LOST -> ":("
      ACTIVE -> ":)"
  in
    button [class "reset", text buttonText]

remainingFlags : Int -> Html
remainingFlags count =
  div [class "remaining-flags", text count]

view : Address Action -> Model -> Html
view address model =
  div [class "board"] [
    [resetButton]
    [elapsedTime]
    [remainingFlags getRemainingFlags board]
    [rows]
  ]

main : Signal Html
main =
  Signal.map (view actions.address) game

game : Signal Game
game =
  Signal.foldp update initialGame actions.signal

initialGame : Game
initialGame = {
  board = generateBoard 8 8 10 Seed,
  elapsedTime = 0
}


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

type Action
  = NoOp
  | ClickCell Cell


-- update : Action -> Model -> Model
-- update action model =
