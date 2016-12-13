module Day8 exposing (solve, solve2)

import Array
import Array2D
import Regex

type Pixel
  = On
  | Off


type alias Screen = Array2D.Array2D Pixel

type Instruction
  = Rect Int Int
  | RotateRow Int Int
  | RotateColumn Int Int


countLit : Int -> Screen -> Int
countLit count screen =
  case Array2D.getRow 0 screen of
    Just row ->
      countLit (count + countArray row) (Array2D.deleteRow 0 screen)
    Nothing ->
      count


countArray : Array.Array Pixel -> Int
countArray array =
  Array.map (\a -> if a == On then 1 else 0) array
  |> Array.foldr (+) 0


pattern = Regex.regex "(x|y)=(\\d+) by (\\d+)"


parseLine : String -> Result String Instruction
parseLine input =
  if String.startsWith "rect" input then
    case String.split " " input of
      _::dims::_ ->
        toRect <| String.split "x" dims
      _ ->
        Err "Pattern doesn't match."
  else
    toRotation input


toRotation : String -> Result String Instruction
toRotation input =
  case Regex.find (Regex.AtMost 1) pattern input of
    { submatches }::_ ->
      case submatches of
        Just orientation::Just a::Just b::_ ->
          case (String.toInt a, String.toInt b) of
            (Ok position, Ok shift) ->
              if orientation == "y" then
                Ok <| RotateRow position shift
              else if orientation == "x" then
                Ok <| RotateColumn  position shift
              else
                Err "Invalid pattern"
            _ ->
              Err "Couldn't parse position and shift"
        _ ->
          Err "Invalid pattern"
    _ ->
      Err "Invalid pattern"


toRect : List String -> Result String Instruction
toRect dims =
  case dims of
    col::row::_ ->
      case (String.toInt row, String.toInt col) of
        (Ok rowInt, Ok colInt) ->
          Rect rowInt colInt
          |> Ok
        _ ->
          Err "Could not parse int"
    _ ->
      Err "Not enough dimensions"


screen : Int -> Int -> Screen
screen rows columns =
  Array2D.repeat rows columns Off


rect : Int -> Int -> Screen -> Screen
rect rows columns screen =
  Array2D.indexedMap (\row column cell -> if row < rows && column < columns then On else cell) screen


rotateRow : Int -> Int -> Screen -> Screen
rotateRow row columns screen =
  case Array2D.getRow row screen of
    Just oldRow ->
      let
        newRow = rotateArray columns oldRow
      in
        Array2D.indexedMap(\r c pixel -> if r == row then (Maybe.withDefault Off (Array.get c newRow)) else pixel) screen
    Nothing ->
      screen


rotateColumn : Int -> Int -> Screen -> Screen
rotateColumn column rows screen =
  let
    newColumn = rotateArray rows <| Array.map (Maybe.withDefault Off) <| (Array2D.getColumn column screen)
  in
    Array2D.indexedMap(\r c pixel -> if c == column then (Maybe.withDefault Off (Array.get r newColumn)) else pixel) screen



rotateArray : Int -> Array.Array Pixel -> Array.Array Pixel
rotateArray shift array =
  if shift == 0 then
    array
  else
    case Array.get ((Array.length array) - 1) array of
      Just cell ->
        Array.append (Array.fromList [cell]) (Array.slice 0 ((Array.length array) - 1) array)
        |> rotateArray (shift - 1)
      _ ->
        array


solve : String -> String
solve input =
  String.lines input
  |> List.filterMap (\a -> parseLine a |> Result.toMaybe)
  |> process (screen 6 50)
  |> countLit 0
  |> toString

solve2 : String -> String
solve2 input =
  String.lines input
  |> List.filterMap (\a -> parseLine a |> Result.toMaybe)
  |> process (screen 6 50)
  |> draw ""


draw : String -> Screen -> String
draw message screen =
  case Array2D.getRow 0 screen of
    Just row ->
      draw (message ++ (Array.foldl (\p accum -> if p == On then accum ++ "*" else accum ++ ".") "||||" row)) (Array2D.deleteRow 0 screen)
    Nothing ->
      message


process : Screen -> List Instruction -> Screen
process screen instructions =
  case instructions of
    head::tail ->
      case head of
        Rect x y ->
          process (rect x y screen) tail
        RotateRow row shift ->
          process (rotateRow row shift screen) tail
        RotateColumn column shift ->
          process (rotateColumn column shift screen) tail
    [] ->
      screen

{-}
.**..****.***..*..*.***..****.***....**.***...***.
*..*.*....*..*.*..*.*..*....*.*..*....*.*..*.*....
*..*.***..***..*..*.*..*...*..***.....*.*..*.*....
****.*....*..*.*..*.***...*...*..*....*.***...**..
*..*.*....*..*.*..*.*....*....*..*.*..*.*.......*.
*..*.*....***...**..*....****.***...**..*....***..
-}