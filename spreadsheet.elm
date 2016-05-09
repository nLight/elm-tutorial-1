module Spreadsheet (..) where

import Html exposing (..)
import Html.Attributes exposing (id, class, value)
import Html.Events exposing (on, onFocus, onBlur, targetValue)
import StartApp.Simple exposing (start)
import Array exposing (Array)
import Maybe
import Char
import String
import Dict exposing (Dict)
import Regex exposing (..)


type Either a b
  = Left a
  | Right b


type alias Coords =
  ( Int, Int )


type alias CellModel =
  Either Float String


type alias Model =
  { values : Dict String CellModel
  , focused : Maybe Coords
  }


type Action
  = UpdateCell (Maybe Coords) String
  | Focus Coords


defaultSize : number
defaultSize =
  20


convertValue : String -> CellModel
convertValue val =
  case String.toFloat val of
    Ok floatVal ->
      Left floatVal

    Err _ ->
      Right val


update : Action -> Model -> Model
update action model =
  case action of
    Focus coords ->
      { model | focused = Just coords }

    UpdateCell (Just ( i, j )) val ->
      let
        val' =
          convertValue val

        index =
          getIndex ( i, j )

        values =
          Dict.insert index val' model.values
      in
        { model | values = values }

    _ ->
      model



{-
Т.к. значение ячейки - это Either, нам нужно его правильно распаковать
Если в ячейке число - преобразуем его в строку
Если там строка - попробуем вычислить формулу, иначе вернем саму строку
-}


extractValue : Model -> Coords -> Maybe CellModel -> String
extractValue model coords cellValue =
  case cellValue of
    Just (Left value) ->
      toString value

    Just (Right str) ->
      evalFormula model coords str

    _ ->
      ""



{-
Вычисление формулы
Ищем максимум одно совпадение и вычисляем формулу, если совпадение найдено
-}


evalFormula : Model -> Coords -> String -> String
evalFormula model coords formula =
  let
    matches =
      find (AtMost 1) (regex "^=(sum|mul|div)\\(([A-Z]+\\d+)\\,([A-Z]+\\d+)\\)$") formula
  in
    case matches of
      [ match ] ->
        extractValue model coords (Just (evalMatch model match))

      _ ->
        formula



{-
match представляет из себя List Maybe String
нужно преобразовать индексы ячеек в Int
и по индексам достать значения из ячеек
-}


evalMatch : Model -> Match -> CellModel
evalMatch model match =
  case match.submatches of
    [ Just op, Just index1, Just index2 ] ->
      let
        cell1 =
          getCellValByIndex model index1

        cell2 =
          getCellValByIndex model index2
      in
        case [ cell1, cell2 ] of
          [ Just (Left c1), Just (Left c2) ] ->
            applyOp op c1 c2

          _ ->
            Right (toString [ cell1, cell2 ])

    _ ->
      Right "Error! Wrong match."


applyOp : String -> Float -> Float -> CellModel
applyOp op c1 c2 =
  case op of
    "sum" ->
      Left (c1 + c2)

    "mul" ->
      Left (c1 * c2)

    "div" ->
      Left (c1 / c2)

    _ ->
      Right "Error! Unknown operator."


getIndex : Coords -> String
getIndex coords =
  let
    i =
      fst coords + 1

    j =
      snd coords + 1
  in
    String.join "" [ toLiteral j, toString i ]


getCellVal : Model -> Coords -> Maybe CellModel
getCellVal model coords =
  getCellValByIndex model (getIndex coords)


getCellValByIndex : Model -> String -> Maybe CellModel
getCellValByIndex model index =
  Dict.get index model.values


getFocusedValue : Model -> String
getFocusedValue model =
  case model.focused of
    Just coords ->
      case getCellVal model coords of
        Just (Left val) ->
          toString val

        Just (Right str) ->
          str

        _ ->
          ""

    _ ->
      ""



{-
Разберем обработчик события

  on "input" targetValue (\value -> Signal.message address (UpdateCell i j value))

  По событию "input" взять target.value из объекта event
  и вызвать обработчик типа UpdateCell с координатами ячейки и новым значением (target.value) для нее

  address, i, j - уже известны, это аргументы функции cell, они не изменятся.
  Упростим запись сгруппировав уже известные переменные под новыми функциями:

    on "input" targetValue (\value -> messageToAddress (updateCellIJ value))

  с использованием оператора обратной композиции это можно записать, как

    on "input" targetValue (messageToAddress << updateCellIJ)

  (messageToAddress << updateCellIJ) здесь все так же возвращает функицю,
  которая ждет на вход CellModel и возвращает Message

  Если мы вернемся к полной записи функций, то это будет выглядеть вот так:

    on "input" targetValue (Signal.message address << UpdateCell i j)

Разберем, как это работает

Для начала посмотрим на сигнатуру конструктора обработчика событий

  on : String -> Json.Decode.Decoder a -> a -> Signal.Message

  on "input" targetValue sendMessage -- совсем, как в Javascript

Первый аргумент типа String
    - имя события, в нашем случае это "input"
Второй аргумент типа Json.Decode.Decoder a
    - функция, которая достанет нужные нам данные из объекта события
    - в нашем случае это targetValue, вернет поле value из объекта, на котором произойтет событие
Третий аргумент sendMessage : a -> Signal.Message - функция, которая должна вернуть Signal.Message

У нас есть все кроме функции sendMessage

Чтобы ее получить нам нужно использовать 2 функции:
  1. Signal.message : Address a -> a -> Message
      - это полиморфная функция из стандартной библиотеки, в нашем случае переменная 'a' имеет тип Action
     Signal.message : Address Action -> Action -> Message
  2. UpdateCell : Int -> Int -> CellModel -> Action
      - наша собственная функция, которая возвращает переменную типа Action, который мы определили выше

По-русски:
  1. Signal.message - функция, принимает Address, Action и возвращает Message
  2. UpdateCell - функция, принимает i, j, value и возвращает тип Action

Как мы видим, вторая функция возвращает тип Action,
а первая ожидает переменную типа Action в качестве аргумента,
так же вы видим, что нам известно все кроме value типа CellModel

Попробуем скомпоновать две эти функции так, чтобы оставить снаружи только CellModel

Наша цель получить функцию с сигнатурой
  sendMessage : CellModel -> Message

Частично применяем 1. функцию, чтобы замкнуть address:

  messageToAddress : Action -> Message
  messageToAddress = Signal.message address

Частично применям 2. функцию, чтобы замнкуть уже известные i и j

  updateCellIJ : CellModel -> Action
  updateCellIJ = UpdateCell i j

И того у нас получилось 2 новых функции messageToAddress и updateCellIJ, скомпонуем их:

  sendMessage : CellModel -> Message
  sendMessage value = messageToAddress << updateCellIJ value

Такой формат записи равнозначен

  sendMessage(x) = messageToAddress(updateCellIJ(x))

Обратная композиция << наших функций в развернутом виде

  (Address Action -> Action -> Message) ->
    (Int -> Int -> CellModel -> Action) ->
    (Int -> Int -> CellModel -> Address Action -> Message)
-}


cell : Model -> Signal.Address Action -> Coords -> Html
cell model address coords =
  let
    cellVal =
      Dict.get (getIndex coords) model.values
  in
    td
      []
      [ input
          [ value (extractValue model coords cellVal)
          , on "input" targetValue (Signal.message address << UpdateCell (Just coords))
          , onFocus address (Focus coords)
          ]
          []
      ]


row : Model -> Signal.Address Action -> Int -> Html
row model address i =
  let
    sizeArray =
      Array.repeat defaultSize 0

    cells =
      Array.indexedMap (\j el -> cell model address ( i, j )) sizeArray |> Array.toList
  in
    tr
      []
      (th [] [ text (toString (i + 1)) ] :: cells)


header : Model -> Html
header model =
  let
    r =
      Array.repeat defaultSize 0
  in
    tr
      []
      (td [] [] :: ((Array.indexedMap (\i a -> th [] [ text (toLiteral (i + 1)) ]) r) |> Array.toList))


sheet : Signal.Address Action -> Model -> Html
sheet address model =
  let
    sheetArray =
      Array.repeat defaultSize 0
  in
    table
      [ class "table" ]
      [ tbody
          []
          (List.append
            [ header model ]
            (Array.indexedMap (\i el -> row model address i) sheetArray |> Array.toList)
          )
      ]


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "container" ]
    [ input
        [ value (getFocusedValue model)
        , on "input" targetValue (Signal.message address << UpdateCell model.focused)
        ]
        []
    , sheet address model
    ]


fromLiteral : String -> Int
fromLiteral str =
  String.toList str |> List.map Char.toCode |> List.foldr (+) 0


toLiteral : Int -> String
toLiteral i =
  toLiteral' "" i


toLiteral' : String -> Int -> String
toLiteral' acc i =
  case i of
    0 ->
      acc

    _ ->
      let
        modulo =
          (i - 1) % 26

        name =
          (Char.fromCode (65 + modulo) |> String.fromChar) ++ acc

        i' =
          (i - modulo) // 26
      in
        toLiteral' name i'


model : Model
model =
  { values = Dict.fromList [ ( "A1", (Left 1) ), ( "B2", (Left 10) ), ( "C3", Right "=sum(A1,B2)" ) ]
  , focused = Nothing
  }


main : Signal Html
main =
  start { model = model, update = update, view = view }
