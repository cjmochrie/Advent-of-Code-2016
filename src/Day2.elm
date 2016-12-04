module Day2 exposing (..)

import String
import Basics
import Maybe exposing (andThen)

type Instruction
  = Up
  | Down
  | Left
  | Right


type Position
  = Num Int
  | A
  | B
  | C
  | D

toString : Position -> String
toString position =
  case position of
    Num num -> Basics.toString num
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"

basicMove : Instruction -> Position -> Position
basicMove instruction position =
  case position of
    Num num ->
      let newNum =
        case instruction of
          Up ->
            if num > 3 then num - 3 else num
          Down ->
            if num < 7 then num + 3 else num
          Left ->
            if num /= 1 && num /= 4 && num /= 7 then
              num - 1
            else
              num
          Right ->
            if num % 3  /= 0 then
              num + 1
            else
              num
        in
          Num newNum
    _ ->  position


advancedMove : Instruction -> Position -> Position
advancedMove instruction position =
  case instruction of
    Up ->
      case position of
        Num 6 -> Num 2
        Num 3 -> Num 1
        Num 7 -> Num 3
        Num 8 -> Num 4
        A -> Num 6
        B -> Num 7
        C -> Num 8
        D -> B
        _ -> position
    Down ->
      case position of
        Num 1 -> Num 3
        Num 2 -> Num 6
        Num 3 -> Num 7
        Num 4 -> Num 8
        Num 6 -> A
        Num 7 -> B
        Num 8 -> C
        B -> D
        _ -> position
    Left ->
      case position of
        Num num ->
         if num /= 1 && num /= 2 && num /= 5 then Num (num - 1) else position
        B -> A
        C -> B
        _ -> position
    Right ->
      case position of
        Num num ->
          if num /= 1 && num /= 4 && num /= 9 then Num (num + 1) else position
        A -> B
        B -> C
        _ -> position


moves : (Instruction -> Position -> Position) -> List Instruction -> Position -> Position
moves move instructions position =
  case instructions of
    head :: tail ->
      move head position
      |> moves move tail
    [] ->
      position


parseInstruction : Char -> Maybe Instruction
parseInstruction char =
  case char of
    'U' -> Just Up
    'D' -> Just Down
    'L' -> Just Left
    'R' -> Just Right
    _ -> Nothing


parseLine : String -> List Instruction
parseLine line =
  String.toList line
  |> List.filterMap parseInstruction


parseInstructions : String -> List (List Instruction)
parseInstructions input =
  String.lines input
  |> List.map parseLine


simulate : (Instruction -> Position -> Position) -> Position -> List (List Instruction) -> String -> String
simulate moveFunc start instructions history =
  case instructions of
    head :: tail ->
      let nextPosition =
        moves moveFunc head start
      in
        simulate moveFunc nextPosition tail (history ++ toString nextPosition)
    [] ->
      history


generalSolve : (Instruction -> Position -> Position) -> String -> String
generalSolve moveFunc input =
  let instructions =
    parseInstructions input
  in
    simulate moveFunc (Num 5) instructions ""


solve = generalSolve basicMove
solve2 = generalSolve advancedMove