import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import ElmTextSearch

-- todo
-- render within custom index.html
-- styling
-- try with real json data for FAQ

main =
  StartApp.start { model = initialModelWithSearchResults, view = view, update = update }

initialModelWithSearchResults =
  let
    results = (search initialModel initialModel.query)
  in
    { initialModel | results = results }


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

search model text =
  searchIds model text
  |> List.filterMap (findHelpItem model)

searchIds model text =
  let
    result = searchAndReturnIndex model text
    |> Result.map snd
  in
   case result of
     Ok hits -> List.map fst hits
     Result.Err _ -> []

findHelpItem : Model -> String -> Maybe HelpItem
findHelpItem model foundId =
  List.filter (\helpItem -> toString(helpItem.id) == foundId) model.helpItems
  |> List.head


---- MODEL ----

initialModel : Model
initialModel =
  { helpItems =
    [ { id = 1, question = "Question1", answer = "Answer1" }
    , { id = 2, question = "Question2", answer = "Answer2" }
    ]
  , results = []
  , query = "Q"
  }

type alias Model =
  { helpItems : List HelpItem
  , results : List HelpItem
  , query : String
  }


type alias HelpItem =
  { id : Int
  , question : String
  , answer : String
  }


---- Full text search indexing ----

searchAndReturnIndex model text =
  buildSearchIndex(model)
  |> ElmTextSearch.search text

buildSearchIndex model =
  (ElmTextSearch.addDocs model.helpItems createNewIndex)
  |> fst

createNewIndex : ElmTextSearch.Index HelpItem
createNewIndex =
  ElmTextSearch.new
    { ref = (\helpItem -> helpItem.id |> toString)
    , fields =
        [ ( .question, 1.0 )
        , ( .answer, 1.0 )
        ]
    }
