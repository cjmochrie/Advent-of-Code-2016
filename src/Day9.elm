module Day9 exposing (solve, solve2)

import Regex

type alias Code =
  { length: Int
  , chars: Int
  , repeat: Int
  }


pattern = Regex.regex "\\((\\d+x\\d+)\\).*"


parseCode : String -> Maybe Code
parseCode input =
  case String.split "x" input of
    a::b::_ ->
      case (String.toInt a, String.toInt b) of
        (Ok chars, Ok repeat) ->
          Just { length = String.length input + 2, chars = chars, repeat = repeat }
        _ ->
          Nothing
    _ ->
      Nothing


extractCode : String -> Maybe (Int, Code)
extractCode input =
  case Regex.find (Regex.AtMost 1) pattern input of
    { submatches, index }::_ ->
      case submatches of
        Just codeString::_ ->
          case parseCode codeString of
            Just code ->
              Just (index, code)
            _ ->
              Nothing
        _ ->
          Nothing
    _ ->
      Nothing


decompress: String -> String
decompress input =
  case extractCode input of
    Just (index, code) ->
      let
        prefix =  String.left index input
        postfix = String.dropLeft (index + code.length + code.chars) input
      in
        prefix ++ String.repeat code.repeat (String.slice (index + code.length) (index + code.length + code.chars) input) ++ decompress postfix
    _ ->
      input


decompress2: String -> String
decompress2 input =
  case extractCode input of
    Just (index, code) ->
      let
        prefix =  String.left index input
        postfix = String.dropLeft (index + code.length + code.chars) input
      in
        prefix ++ decompress2 (String.repeat code.repeat (String.slice (index + code.length) (index + code.length + code.chars) input) ++ postfix)
    _ ->
      input


solve: String -> String
solve input =
  String.filter (\c -> c /= ' ' && c /= '\n') input
  |> decompress
  |> String.length
  |> toString


solve2: String -> String
solve2 input =
  String.filter (\c -> c /= ' ' && c /= '\n') input
  |> decompress2
  |> String.length
  |> toString