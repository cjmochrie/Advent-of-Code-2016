module Day15 exposing (..)

import Regex


type alias Disc = (Int, Int)


type alias Time = Int


open : Disc -> Time -> Bool
open (positions, start) time =
  0 == (start + time) % positions


pass : List Disc -> Time -> Bool
pass discs time =
  List.indexedMap (\position disc -> open disc (time + position + 1)) discs
  |> List.all (\a -> a == True)


firstPass : List Disc -> Time -> Time
firstPass discs time =
  if pass discs time then
    time
  else
    firstPass discs (time + 1)


pattern = Regex.regex "Disc #\\d+ has (\\d+) positions; at time=0, it is at position (\\d+)."


parseDisc : String -> Maybe Disc
parseDisc input =
  case Regex.find (Regex.AtMost 1) pattern input of
    { submatches }::_ ->
      case submatches of
        [Just positions, Just start] ->
          case (String.toInt positions, String.toInt start) of
            (Ok pos, Ok st) ->
              Just (pos, st)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing


parseDiscs : String -> List Disc
parseDiscs input =
  String.lines input
  |> List.filterMap parseDisc


solve : String -> String
solve input =
  let discs =
    parseDiscs input
  in
    firstPass discs 0
    |> toString
