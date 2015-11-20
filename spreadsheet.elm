module Spreadsheet where
import Html exposing (..)
import Html.Attributes exposing (id, class)
import StartApp.Simple exposing (start)
import Array exposing (Array)


type alias CellModel = Float
type alias RowModel = Array CellModel
type alias Model = Array RowModel

type Action
  = NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model


cell : Signal.Address Action -> Int -> Int -> CellModel -> Html
cell address i j data =
  td [] [ text (toString data) ]


row : Signal.Address Action -> Int -> RowModel -> Html
row address i data =
  tr []
  (Array.indexedMap (cell address i) data |> Array.toList)


view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "container" ]
  [ table [ class "table" ]
    [ tbody []
      (Array.indexedMap (row address) model |> Array.toList)
    ]
  ]


model : Model
model = Array.fromList
  [ Array.fromList [1, 2, 3, 4]
  , Array.fromList [1, 2, 3, 4]
  ]

main : Signal Html
main = start { model = model , update = update , view = view }
