module Day7 exposing (solve, solve2)

import Regex


type alias Address =
  { base: List String
  , hypernet: List String
  }


type alias ABA = (Char, Char)


address : (List (Maybe String), List (Maybe String)) -> Address
address (base, hypernet) =
  { base = List.filterMap identity base
  , hypernet = List.filterMap identity hypernet
  }


solve : String -> String
solve input =
  performSolve input tls


solve2 : String -> String
solve2 input =
  performSolve input ssl


performSolve : String -> (Address -> Bool) -> String
performSolve input testFunction =
  String.lines input
  |> List.filterMap parseLine
  |> List.foldl (\el count -> if testFunction el then count + 1 else count) 0
  |> toString


tls : Address -> Bool
tls address =
  anyAbba address.base && not (anyAbba address.hypernet)


ssl : Address -> Bool
ssl address =
  let
    baseABAs = allABAs address.base
    hypernetABAs = allABAs address.hypernet
  in
    List.any (\(first, second) -> List.member (second, first) hypernetABAs) baseABAs


allABAs : List String -> List ABA
allABAs input =
  List.map (\el -> findABAs (String.toList el) []) input
  |> List.concat


findABAs : List Char -> List ABA -> List ABA
findABAs chars abas =
  case chars of
    a::b::c::rest ->
      if a == c && a /=b then
        findABAs ([b] ++ [c] ++ rest) (abas ++ [(a, b)])
      else
        findABAs ([b] ++ [c] ++ rest) abas
    _ ->
      abas


anyAbba : List String -> Bool
anyAbba strings =
  case strings of
    head::tail ->
      (abba <| String.toList head) || anyAbba tail
    [] ->
      False


-- returns true if List of chars contains pattern
abba : List Char -> Bool
abba chars =
  case chars of
    a::b::c::d::rest ->
      if a == d && b == c && a /= b then
        True
      else
        abba ([b] ++ [c] ++ [d] ++ rest)
    _ ->
      False


pattern = Regex.regex "(\\w+)*(\\[(\\w+)\\])*"


parseLine : String -> Maybe Address
parseLine input =
  Regex.find Regex.All pattern input
    |> List.map (\{ submatches } -> extractGroups submatches)
    |> List.unzip
    |> Just << address



extractGroups : List (Maybe String) -> (Maybe String, Maybe String)
extractGroups submatches =
  case submatches of
    a::_::c::_ ->
      (a, c)
    _ ->
      (Nothing, Nothing)
