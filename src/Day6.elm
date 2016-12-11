module Day6 exposing (..)

import Dict
import Char
import String


type alias CharCounter =
  Dict.Dict Char Int

solve : String -> String
solve input =
  performSolve input <| performSort (\(char, count) -> -count)


solve2 : String -> String
solve2 input =
  performSolve input <| performSort (\(char, count) -> count)


performSolve : String -> (CharCounter -> Maybe Char) -> String
performSolve input comparator =
  let
    lines = String.lines input
  in
    case lines of
      [] -> ""
      head::tail ->
        List.repeat (String.length head) Dict.empty
        |> countChars lines
        |> List.filterMap comparator
        |> String.fromList


countChars : List String -> List CharCounter -> List CharCounter
countChars input dicts =
  case input of
    [] ->
      dicts
    head::tail ->
      countChars tail (counter head dicts)


-- Take a string and list of dicts. increment character count with
-- each position being index of list
counter : String -> List CharCounter -> List CharCounter
counter input dicts =
  let chars =
    String.toList input
  in
    List.map2 (\char dict -> Dict.update char increment dict) chars dicts


-- Return most first char in the char counter after sorting by the comparator
performSort : ((Char, Int) -> Int) -> CharCounter -> Maybe Char
performSort comparator dict =
  let
    sortedList =
      Dict.toList dict
      |> List.sortBy comparator
  in
    case sortedList of
      (char, count)::tail ->
        Just char
      [] ->
        Nothing


increment : Maybe Int -> Maybe Int
increment count =
  case count of
    Just num -> Just (num + 1)
    Nothing -> Just 1
