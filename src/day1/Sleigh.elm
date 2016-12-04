module Sleigh exposing (..)

import String exposing (..)
import List
import Debug
import Result
import Set


type Bearing
  = North
  | East
  | South
  | West

type Direction
  = Left
  | Right
  | Straight

type alias Position =
  { x : Int
  , y : Int
  }


parsePlan : String -> List Direction
parsePlan input =
  split "," input
  |> List.concatMap parseDirection


parseTurn : String -> Direction
parseTurn input =
  case input of
    "L" -> Left
    _ -> Right

parseDirection : String -> List Direction
parseDirection input =
  let rawInstr =
    trim input
  in
    let
      turn = parseTurn <| left 1 rawInstr
      distance = Result.withDefault 0 (toInt <| dropLeft 1 rawInstr)
    in
      [turn] ++ List.repeat (distance - 1) Straight


zero = { x = 0, y = 0 }

type alias Sleigh =
  { bearing : Bearing
  , coordinates : Position
  }

sleighMaker : Bearing -> Position -> Sleigh
sleighMaker bearing position =
  { bearing = bearing
  , coordinates = position
  }

start = sleighMaker North zero

turn : Direction -> Bearing -> Bearing
turn direction bearing =
  case direction of
    Right ->
      case bearing of
        North -> East
        East -> South
        South -> West
        West -> North
    Left ->
      case bearing of
        North -> West
        West -> South
        South -> East
        East -> North
    Straight ->
      bearing


advance : Sleigh -> Direction -> Sleigh
advance sleigh direction =
  case direction of
    Straight ->
      { sleigh | coordinates = advanceOne sleigh.bearing sleigh.coordinates }
    _ ->
      let
        newBearing = turn direction sleigh.bearing
      in
        { bearing = newBearing, coordinates = advanceOne newBearing sleigh.coordinates }


advanceOne : Bearing -> Position -> Position
advanceOne bearing { x, y } =
    case bearing of
      North ->
        { x = x, y = y + 1 }
      East ->
        { x = x + 1, y = y }
      South ->
        { x = x, y = y - 1 }
      West ->
        { x = x - 1, y = y }


simulate : List Direction -> Sleigh -> Sleigh
simulate plan sleigh =
  case plan of
    head :: tail -> simulate tail <| advance sleigh head
    [] -> sleigh


runSimulation : String -> String
runSimulation directions =
  let { bearing, coordinates} =
    simulate (parsePlan directions) start
  in
    let _ =
      Debug.log "x, y" (coordinates.x, coordinates.y)
    in
      abs coordinates.x + abs coordinates.y
      |> toString


runVisitTwice : String -> String
runVisitTwice directions =
  let { bearing, coordinates} =
    visitedTwice (parsePlan directions) (Set.singleton (0, 0)) start
  in
    let _ =
      Debug.log "x, y" (coordinates.x, coordinates.y)
    in
      abs coordinates.x + abs coordinates.y
      |> toString



visitedTwice : List Direction -> Set.Set (Int, Int) -> Sleigh -> Sleigh
visitedTwice plan locations sleigh =
  case plan of
    head :: tail ->
      let
        next = advance sleigh head
      in
        if Set.member (next.coordinates.x, next.coordinates.y) locations then
          next
        else
          visitedTwice tail (Set.insert (next.coordinates.x, next.coordinates.y) locations) next
    [] ->
      sleigh