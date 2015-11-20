module Spreadsheet where
import Html exposing (..)
import Html.Attributes exposing (id, class, value)
import Html.Events exposing (on, targetValue)
import StartApp.Simple exposing (start)
import Array exposing (Array)
import Maybe exposing (withDefault)
import Char
import String


type Either a b
    = Left a
    | Right b

type alias CellModel = Either Float String

type alias RowModel = Array CellModel
type alias Model = Array RowModel

type Action
  = NoOp
  | UpdateCell Int Int String


update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    UpdateCell i j val ->
      let
        r  = withDefault Array.empty (Array.get i model)
        r' = Array.set j (Right val) r
      in
        Array.set i r' model


extractValue : CellModel -> String
extractValue m =
  case m of
    Left a -> toString a
    Right b -> b


cell : Signal.Address Action -> Int -> Int -> CellModel -> Html
cell address i j data =
  td []
  [ input
    [ value (extractValue data)
    , on "input" targetValue (Signal.message address << UpdateCell i j)
    ] []
  ]


row : Signal.Address Action -> Int -> RowModel -> Html
row address i data =
  tr []
  (th [] [ text (toString (i + 1)) ] :: (Array.indexedMap (cell address i) data |> Array.toList))


toLiteral : Int -> String
toLiteral i =
  toLiteral' "" i


toLiteral' acc i =
  case i of
    0 -> acc
    _ ->
      let
        modulo = (i - 1) % 26
        name = (Char.fromCode (65 + modulo) |> String.fromChar) ++ acc
        i' = (i - modulo) // 26
      in
        toLiteral' name i'


header : Model -> Html
header model =
  let
    r = withDefault Array.empty (Array.get 0 model)
  in
    tr []
    (td [][] :: ((Array.indexedMap (\i a -> th [] [text (toLiteral (i+1))]) r) |> Array.toList))


view : Signal.Address Action -> Model -> Html
view address model =
  table [ class "table" ]
    [ tbody []
      (List.append
        [header model]
        (Array.indexedMap (row address) model |> Array.toList))
    ]


model : Model
model = Array.fromList
  [ Array.fromList (List.repeat 50 (Left 1))
  , Array.fromList (List.repeat 50 (Left 2))
  ]

main : Signal Html
main = start { model = model , update = update , view = view }
