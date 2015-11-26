module Minesweeper where

import Array
import Board
import Cell exposing (Cell)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Row exposing (Row)
import Random exposing (initialSeed, Seed)
import Signal exposing (Signal, Address)
import String
import Time exposing (Time)
import Window

type GameStatus = WON | LOST | ACTIVE

type alias Model = {
  board: Board.Board,
  elapsedTime: Time
}

getGameStatus : Model -> GameStatus
getGameStatus game =
  WON

generateBoard : Int -> Int -> Int -> Seed -> Board.Board
generateBoard rows columns mines seed =
  placeMines (generateEmptyBoard rows columns) mines seed

generateEmptyBoard : Int -> Int -> Board.Board
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
    status = Cell.NORMAL
  }

placeMines : Board.Board -> Int -> Seed -> Board.Board
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

getRemainingFlags : Board.Board -> Int
getRemainingFlags board =
  1

view : Address Action -> Model -> Html
view address model =
  div [class "board"] [
    (resetButton model),
    (elapsedTime model),
    (remainingFlags model),
    (Board.view model.board)
  ]

main : Signal Html
main =
  Signal.map (view actions.address) game

game : Signal Model
game =
  Signal.foldp update initialGame actions.signal

initialGame : Model
initialGame = {
    board = generateBoard 8 8 10 (initialSeed 31415),
    elapsedTime = 0
  }


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

type Action
  = NoOp
  | RevealCell Cell
  | FlagCell Cell


update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    RevealCell cell -> model
    FlagCell cell -> model
