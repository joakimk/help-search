import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp

import HelpSearchModel exposing (Model, HelpItem, search)

-- todo
-- render within custom index.html
-- styling
-- try with real json data for FAQ

main =
  StartApp.start { model = initialModelWithSearchResults, view = view, update = update }


---- VIEW ----

view address model =
  div
    [ ]
    [ input [ class "help-search__box"
            , type' "search"
            , placeholder "Skriv här: nyckelord, begrepp, fråga…"
            , value model.query
            , onInput address Search
            ] []
    , div [] (List.map renderResult model.results)
    ]

-- TODO: clarify what "f" and "v" is, etc.
-- borrowed from https://online.pragmaticstudio.com/courses/elm/steps/36
onInput : Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

renderResult : HelpItem -> Html
renderResult helpItem =
  div
  []
  [ text helpItem.question ]


---- UPDATE ----

type Action
  = NoOp
  | Search String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Search text ->
      { model | results = (search model text), query = text }


---- MODEL ----

initialModelWithSearchResults =
  let
    results = (search initialModel initialModel.query)
  in
    { initialModel | results = results }

initialModel : Model
initialModel =
  { helpItems =
    [ { id = 1, question = "What is your favorite color?", answer = "Dunno" }
    , { id = 2, question = "What is a Elm?", answer = "A type of tree" }
    ]
  , results = []
  , query = "color"
  }
