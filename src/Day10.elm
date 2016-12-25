-- Ugleeeee

module Day10 exposing (..)

import Dict


type alias Chip = Int


type alias Bot =
  { lo : Maybe Destination
  , hi : Maybe Destination
  , chips : List Chip
  }

type alias Factory =
  { bots: Dict.Dict Int Bot
  , outputs: Dict.Dict Int Chip
  }


type Destination
  = BotCode Int
  | Output Int


type Instruction
  = Value Chip
  | Transfer Destination Destination


newBot =
  { lo = Nothing
  , hi = Nothing
  , chips = []
  }

newFactory =
  { bots = Dict.empty
  , outputs = Dict.empty
  }


updateBot : Instruction -> Bot -> Bot
updateBot instr bot =
  case instr of
    Value chip ->
      { bot | chips = List.sort (bot.chips ++ [chip]) }
    Transfer lo hi ->
      { bot | lo = Just lo, hi = Just hi }


processBot : (Int, Bot) -> Factory -> Factory
processBot (botCode, bot) factory =
  case (bot.lo, bot.hi) of
    (Just lo, Just hi) ->
      case bot.chips of
        [loChip, hiChip] ->
          let
            newFact = updateFactory hi hiChip (updateFactory lo loChip factory)
            _ = Debug.log "Testing" (botCode, ((toString loChip) == "17" && (toString hiChip) == "61"))
          in
            { newFact | bots = Dict.insert botCode { bot | chips = [] } newFact.bots }
        _ ->
          factory
    _ ->
      factory


updateFactory : Destination -> Chip -> Factory -> Factory
updateFactory destination chip { bots, outputs } =
  case destination of
    BotCode botCode ->
      let newBot =
        Dict.get botCode bots
        |> addChip chip
      in
        let newBots =
          Dict.insert botCode newBot bots
        in
          processBot (botCode, newBot) { outputs = outputs, bots = newBots }
    Output outputCode ->
      { bots = bots, outputs = Dict.insert outputCode chip outputs }


addChip : Chip -> Maybe Bot -> Bot
addChip chip bot =
  case bot of
    Just theBot ->
      { theBot | chips = List.sort (theBot.chips ++ [chip]) }
    Nothing ->
      { newBot | chips = [chip] }



update : (Int, Instruction) -> Factory -> Factory
update (botNumber, instr) factory =
  let
    bot =
      Maybe.withDefault newBot (Dict.get botNumber factory.bots)
      |> updateBot instr
  in
    let newBots = Dict.insert botNumber bot factory.bots
    in
      processBot (botNumber, bot) { factory | bots = newBots }


parseInstruction : List String -> Maybe (Int, Instruction)
parseInstruction input =
  case input of
    "value"::tail ->
      case tail of
        val::_::_::_::code::_ ->
          Just <| (quickInt code, Value (quickInt val))
        _ ->
          Nothing
    "bot"::tail ->
      case tail of
        botCode::_::_::_::lo::loCode::_::_::_::hi::hiCode::_ ->
          (quickInt botCode, Transfer (toTransfer lo loCode) (toTransfer hi hiCode))
          |> Just
        _ ->
          Nothing
    _ ->
      Nothing


toTransfer : String -> String -> Destination
toTransfer destination code =
  if destination == "bot" then
    BotCode <| quickInt code
  else
    Output <| quickInt code


parse : String -> List (Int, Instruction)
parse input =
  String.lines input
  |> List.filterMap (parseInstruction << String.words)


solve : String -> String
solve input =
  let instructions = parse input
  in
    doSolve instructions newFactory
    |> toString


doSolve : List (Int, Instruction) -> Factory -> Factory
doSolve instructions factory =
  case instructions of
    head::tail ->
      update head factory
      |> doSolve tail
    [] ->
      factory


quickInt : String -> Int
quickInt input =
  String.toInt input
  |> Result.withDefault 0

