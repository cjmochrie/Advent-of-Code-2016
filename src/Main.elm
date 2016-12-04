import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Dict

import Sleigh
import Day2
import Day3


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- Problems

problemList =
  [ ("Day 1 Part 1", Sleigh.runSimulation)
  , ("Day 1 Part 2", Sleigh.runVisitTwice)
  , ("Day 2 Part 1", Day2.solve)
  , ("Day 2 Part 2", Day2.solve2)
  , ("Day 3 Part 1", Day3.solve)
  , ("Day 3 Part 2", Day3.solve2)
  ]


problemFunctions =
  Dict.fromList problemList


getFunc : String -> (String -> String)
getFunc name =
  Maybe.withDefault (\a -> a) (Dict.get name problemFunctions)


-- MODEL

type alias ProblemData =
  { name : String
  , input : String
  , output : String
  }

empty name =
  { name = name
  , input = ""
  , output = ""
  }


type alias Model =
  { problems : Dict.Dict String ProblemData
  }


model =
  { problems =
    List.map (\(a, _) -> (a, empty a)) problemList
    |> Dict.fromList
  }
-- UPDATE

type Msg
  = UpdateInput String String
  | UpdateOutput String


update : Msg -> Model -> Model
update msg { problems } =
  case msg of
    UpdateInput name text ->
      case Dict.get name problems of
        Just problem ->
          { problems = Dict.insert name { problem | input = text } problems }
        Nothing ->
          { problems = problems }
    UpdateOutput name ->
      case Dict.get name problems of
        Just problem ->
          { problems = Dict.insert name { problem | output = getFunc name problem.input } problems }
        Nothing ->
          { problems = problems }


-- VIEW

view : Model -> Html Msg
view model =
  div [] (List.map renderProblem <| Dict.values model.problems)


renderProblem : ProblemData -> Html Msg
renderProblem problem =
  div []
  [ h3 [] [ text problem.name ]
  , textarea [ placeholder "Input", onInput (UpdateInput problem.name) ] []
  , button [ onClick (UpdateOutput problem.name) ] [ text "Run" ]
  , span [] [ text "Answer: " ]
  , span [] [ text problem.output ]
  ]