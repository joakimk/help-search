import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import ElmTextSearch

main =
  StartApp.start { model = initialModel, view = view, update = update }

---- VIEW ----
view address model =
  div
    [ ]
    [ input [ class "help-search__box"
            , type' "search"
            , placeholder "Skriv här: nyckelord, begrepp, fråga…"
            , onInput address Search
            ] []
    , div [] (List.map renderResult model.helpItems)
    , div [] [ text model.results ]
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

    -- TODO: implement
    Search text ->
      { model | results = (search model text) }

search model text =
  searchAndReturnIndex model "Q"
  |> Result.map snd
  |> toString

---- MODEL ----

initialModel : Model
initialModel =
  { helpItems =
    [ { id = 1, question = "Question1", answer = "Answer1" }
    , { id = 2, question = "Question2", answer = "Answer2" }
    ]
  , results = ""
  }

type alias Model =
  { helpItems : List HelpItem
  , results : String
  }


type alias HelpItem =
  { id : Int
  , question : String
  , answer : String
  }


---- Full text search indexing ----

searchAndReturnIndex model text =
  buildSearchIndex(model)
    `Result.andThen`
    (ElmTextSearch.search text)

buildSearchIndex model =
  ElmTextSearch.add { id = 3, question = "Question3", answer = "Answer3" } createNewIndexExample
--List.foldl (\helpItem index ElmTextSearch.add(helpItem index)) createNewIndexExample model.helpItems

createNewIndexExample : ElmTextSearch.Index HelpItem
createNewIndexExample =
  ElmTextSearch.new
    { ref = (\helpItem -> helpItem.id |> toString)
    , fields =
        [ ( .question, 1.0 )
        , ( .answer, 1.0 )
        ]
    }
