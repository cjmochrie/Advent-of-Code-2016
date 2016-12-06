module Day5 exposing (..)

import String exposing (..)
import MD5 exposing (hex)


findChar : String -> Int -> Maybe Char
findChar input num =
  let
    hash = hex <| input ++ toString num
  in
    if left 5 hash == "00000" then
      case uncons <| dropLeft 5 hash of
        Just (char, _) -> Just char
        _ -> Nothing
    else
      Nothing


findChars : String -> String -> Int -> String
findChars code password round =
  let _ =
    Debug.log "round" round
  in
    if length password == 1 then
      password
    else
      case findChar code round of
        Just char ->
          findChars code (password ++ toString char) (round + 1)
        Nothing ->
          findChars code password (round + 1)

solve : String -> String
solve input =
  findChars input "" 0
