module Day13 exposing (solve)

import Set


type alias Point = (Int, Int)


toBinary : List Int -> Int -> List Int
toBinary binary num =
  if num < 1 then
    binary
  else
    toBinary ([num % 2] ++ binary) (num // 2)


isOpen : Int -> Point -> Bool
isOpen salt (x, y) =
  if x >= 0 && y >= 0 then
    let sum =
      x*x + 3*x + 2*x*y + y + y*y + salt
      |> toBinary []
      |> List.sum
    in
      (sum % 2) == 0
  else
    False
   

edges : (Point -> Bool) -> Point -> List Point
edges openFunc (x, y) =
  List.filter openFunc [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]


search : (Point -> List Point) -> Point -> Set.Set Point -> Set.Set Point -> Int -> Int
search edgeFunc goal current visited steps =
  if Set.member goal current then
    steps
  else
    let
      nextVisited =
        Set.union current visited
      nextSearch =
        Set.toList current
        |> List.map edgeFunc
        |> List.concat
        |> Set.fromList
    in
      search edgeFunc goal (Set.diff nextSearch nextVisited) nextVisited (steps + 1)


solve : String -> String
solve input =
  case String.toInt input of
    Ok number ->
      search (edges <| isOpen number) (31, 39) (Set.singleton (1, 1)) Set.empty 0
      |> toString
    Err msg ->
      msg