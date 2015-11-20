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


toLiteral' : String -> Int -> String
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
