module Day3 exposing (solve, solve2)

import String exposing (words, join, toInt, lines)

type alias Triangle = (Int, Int, Int)


validate : Triangle -> Bool
validate (x, y, z) =
  x + y > z && x + z > y && y + z > x


parseLine : String -> Maybe Triangle
parseLine input =
  let numbers =
    words input
    |> List.map toInt
  in
    case numbers of
      [Ok a, Ok b, Ok c] -> Just (a, b, c)
      _ -> Nothing


parseInput : String -> List Triangle
parseInput input =
  lines input
  |> List.filterMap parseLine


solve : String -> String
solve input =
  parseInput input
    |> List.filter validate
    |> List.length
    |> toString


solve2 : String -> String
solve2 input =
  words input
  |> reorganize
  |> solve


reorganize : List String -> String
reorganize words =
  case words of
    a1::a2::a3::b1::b2::b3::c1::c2::c3::tail ->
      let reorged =
        [[a1, b1, c1], [a2, b2, c2], [a3, b3, c3]]
        |> List.map (join " ")
        |> join "\n"
      in
        join "\n" [reorged, reorganize tail]

    _ -> ""
