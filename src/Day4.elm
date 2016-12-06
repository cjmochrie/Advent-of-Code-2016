module Day4 exposing (..)

import Regex exposing (..)
import Dict
import Char


type alias Room =
  { name: String
  , id: Int
  , checksum: List Char
  }


decrypt : Room -> Room
decrypt room =
  { room | name = String.map (rotate room.id) room.name }


aCode = Char.toCode 'a'


rotate : Int -> Char -> Char
rotate num char =
  if char == '-' then
    ' '
  else
    let newCode =
      (Char.toCode char - aCode + num) % 26 + aCode
    in
      Char.fromCode newCode


pattern = regex "((\\w+-)+)(\\d+)\\[(\\w{5})\\]"

parseRoom : String -> Maybe Room
parseRoom input =
  case find (AtMost 1) pattern input of
    { submatches }::_ ->
      case submatches of
        Just name::_::Just code::Just checksum::_ ->
          case String.toInt code of
            Ok id ->
              Just
              { name = name
              , id = id
              , checksum = String.toList checksum
              }
            Err _ ->
              Nothing
        _ ->
          Nothing
    [] ->
      Nothing


countComparison : (Char, Int) -> (Char, Int) -> Order
countComparison (a, aCount) (b, bCount) =
  case compare bCount aCount of
    EQ -> compare a b
    _ -> compare bCount aCount


validate : Room -> Bool
validate room =
  let counts =
    String.filter Char.isLower room.name
    |> count
    |> Dict.toList
  in
    List.sortWith countComparison counts
    |> List.map (\(char, count) -> char)
    |> List.take 5
    |> (==) room.checksum


increment : Maybe Int -> Maybe Int
increment count =
  case count of
    Just num -> Just (num + 1)
    Nothing -> Just 1


count_ : List Char -> Dict.Dict Char Int -> Dict.Dict Char Int
count_ chars dict =
  case chars of
    head::tail ->
      count_ tail (Dict.update head increment dict)
    [] -> dict


count : String -> Dict.Dict Char Int
count input =
  count_ (String.toList input) Dict.empty


solve : String -> String
solve input =
  String.lines input
  |> List.filterMap parseRoom
  |> List.filter validate
  |> List.foldr (\room accum -> room.id + accum) 0
  |> toString


solve2 : String -> String
solve2 input =
  String.lines input
  |> List.filterMap parseRoom
  |> List.filter validate
  |> List.map decrypt
  |> List.filter (\room -> String.contains "pole" room.name)
  |> List.map toString
  |> String.join "\n"