module Day12 exposing (..)

import Array

type Register
  = A
  | B
  | C
  | D


type Op
  = CpyVal Int Register
  | CpyReg Register Register
  | Inc Register
  | Dec Register
  | Jnz Register Int
  | Jump Int
  | Noop


type alias Computer =
  { a : Int
  , b : Int
  , c : Int
  , d : Int
  , ops : Array.Array Op
  , pointer : Int
  }


restart : Array.Array Op -> Computer
restart ops =
 { a = 0
 , b = 0
 , c = 0
 , d = 0
 , ops = ops
 , pointer = 0
 }


parseReg : String -> Maybe Register
parseReg input =
  case input of
    "a" -> Just A
    "b" -> Just B
    "c" -> Just C
    "d" -> Just D
    _ -> Nothing


parseOp : String -> Maybe Op
parseOp input =
  case String.words input of
    [op, a, b] ->
      case (parseReg b, String.toInt a, parseReg a, String.toInt b) of
        (Just reg, Ok val, Nothing, Err _) ->
          if op == "cpy" then
            Just <| CpyVal val reg
          else
            Nothing
        (Just reg, Err _, Just source, Err _) ->
          if op == "cpy" then
            Just <| CpyReg source reg
          else
            Nothing
        (Nothing, Err _, Just reg, Ok val) ->
          if op == "jnz" then
            Just <| Jnz reg val
          else
            Nothing
        (Nothing, Ok a, Nothing, Ok b) ->
          if op == "jnz" && a /= 0 then
            Just <| Jump b
          else if op == "jnz" && a == 0 then
            Just Noop
          else
            Nothing
        _ ->
          Nothing
    [op, register] ->
      case parseReg register of
        Just reg ->
          if op == "inc" then
            Just <| Inc reg
          else if op == "dec" then
            Just <| Dec reg
          else
            Nothing
        _ ->
          Nothing
    _ ->
      Nothing


getRegister : Register -> Computer -> Int
getRegister reg computer =
  case reg of
    A -> computer.a
    B -> computer.b
    C -> computer.c
    D -> computer.d


setRegister : Register -> Computer -> Int -> Computer
setRegister reg computer val =
  case reg of
    A -> { computer | a = val }
    B -> { computer | b = val }
    C -> { computer | c = val }
    D -> { computer | d = val }


doJump : Int -> Computer -> Computer
doJump jump computer=
  { computer | pointer = computer.pointer + jump }


next = doJump 1


execute : Computer -> Computer
execute computer =
  case Array.get computer.pointer computer.ops of
    Nothing ->
      computer
    Just op ->
      let nextComputer =
        case op of
          CpyVal val reg ->
            setRegister reg computer val
          CpyReg source reg ->
            getRegister source computer
            |> setRegister reg computer
          Inc reg ->
            getRegister reg computer
            |> (+) 1
            |> setRegister reg computer
          Dec reg ->
            getRegister reg computer
            |> (+) -1
            |> setRegister reg computer
          Jnz reg jump ->
            if getRegister reg computer /= 0 then
              doJump (jump - 1) computer
            else
              computer
          Jump jump ->
            doJump (jump - 1) computer
          Noop ->
            computer
      in
        execute <| next nextComputer

solve : String -> String
solve input =
  String.lines input
  |> List.filterMap parseOp
  |> Array.fromList
  |> restart
  |> execute
  |> .a
  |> toString


solve2 : String -> String
solve2 input =
  let newComputer =
    String.lines input
    |> List.filterMap parseOp
    |> Array.fromList
    |> restart
  in
    setRegister C newComputer 1
    |> execute
    |> .a
    |> toString