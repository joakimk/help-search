module HelpSearchModel
  ( Model, HelpItem, search ) where

import ElmTextSearch

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

search : Model -> String -> List HelpItem
search model text =
  searchIds model text
  |> List.filterMap (findHelpItem model)

---- private ----

searchIds model text =
  let
    result = runHelpItemSearch model text
    |> Result.map snd
  in
   case result of
     Ok hits -> List.map fst hits
     Result.Err _ -> []

findHelpItem : Model -> String -> Maybe HelpItem
findHelpItem model foundId =
  List.filter (\helpItem -> toString(helpItem.id) == foundId) model.helpItems
  |> List.head

runHelpItemSearch model text =
  buildHelpItemIndex(model)
  |> ElmTextSearch.search text

buildHelpItemIndex model =
  (ElmTextSearch.addDocs model.helpItems buildEmptyIndex)
  |> fst

buildEmptyIndex : ElmTextSearch.Index HelpItem
buildEmptyIndex =
  ElmTextSearch.new
    { ref = (\helpItem -> helpItem.id |> toString)
    , fields =
        [ ( .question, 1.0 )
        , ( .answer, 1.0 )
        ]
    }
