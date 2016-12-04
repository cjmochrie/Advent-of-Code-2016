module Day3 exposing (..)

type alias Triangle = (Int, Int, Int)


validate : Triangle -> Bool
validate (x, y, z) =
  x + y > z && x + z > y && y + z > x


parseLine : String -> Maybe Triangle
parseLine input =
  let numbers =
    String.words input
    |> List.map String.toInt
  in
    case numbers of
      [Ok a, Ok b, Ok c] -> Just (a, b, c)
      _ -> Nothing


parseInput : String -> List Triangle
parseInput input =
  String.lines input
  |> List.filterMap parseLine


solve : String -> String
solve input =
  parseInput input
    |> List.filter validate
    |> List.length
    |> toString