module Spreadsheet (..) where

import Html exposing (..)
import Html.Attributes exposing (id, class, value)
import Html.Events exposing (on, onFocus, onBlur, targetValue)
import StartApp.Simple exposing (start)
import Array exposing (Array)
import Maybe exposing (withDefault)
import Char
import String
import Regex exposing (..)


type Either a b
  = Left a
  | Right b


type alias CellModel =
  Either Float String


type alias RowModel =
  Array CellModel


type alias Model =
  { values : Array RowModel
  , focused : ( Int, Int )
  }


type Action
  = UpdateCell Int Int String
  | Focus Int Int
  | Blur Int Int


convertValue : String -> CellModel
convertValue val =
  case String.toFloat val of
    Ok v ->
      Left v

    Err _ ->
      Right val


update : Action -> Model -> Model
update action model =
  case action of
    Focus i j ->
      { model | focused = ( i, j ) }

    Blur i j ->
      { model | focused = ( -1, -1 ) }

    UpdateCell i j val ->
      let
        val' =
          convertValue val

        row =
          withDefault Array.empty <| Array.get i model.values

        row' =
          Array.set j val' row

        values =
          Array.set i row' model.values
      in
        { model | values = values }



{-
Т.к. значение ячейки - это Either, нам нужно его правильно распаковать
Если в ячейке число - преобразуем его в строку
Если там строка - попробуем вычислить формулу, иначе вернем саму строку
-}


extractValue : Model -> Int -> Int -> CellModel -> String
extractValue model i j m =
  case m of
    Left a ->
      toString a

    Right b ->
      case Regex.contains (Regex.regex "^=") b of
        True ->
          case model.focused of
            ( i', j' ) ->
              if (i == i') && (j == j') then
                b
              else
                extractValue model i j (evalFormula model b)

        False ->
          b



{-
Вычисление формулы
Ищем максимум одно совпадение и вычисляем формулу, если совпадение найдено
-}


evalFormula : Model -> String -> CellModel
evalFormula model formula =
  let
    matches =
      find (AtMost 1) (regex "^=(sum|mul|div)\\((\\d+):(\\d+)\\,(\\d+):(\\d+)\\)$") formula
  in
    case matches of
      [ match ] ->
        evalMatch model match

      _ ->
        Right formula



{-
match представляет из себя List Maybe String
нужно преобразовать индексы ячеек в Int
и по индексам достать значения из ячеек
-}


evalMatch : Model -> Match -> CellModel
evalMatch model match =
  case match.submatches of
    [ Just op, Just i1, Just j1, Just i2, Just j2 ] ->
      let
        i1' =
          Result.withDefault 0 (String.toInt i1)

        j1' =
          Result.withDefault 0 (String.toInt j1)

        i2' =
          Result.withDefault 0 (String.toInt i2)

        j2' =
          Result.withDefault 0 (String.toInt j2)

        cell1 =
          getCellVal model i1' j1'

        cell2 =
          getCellVal model i2' j2'
      in
        case [ cell1, cell2 ] of
          [ Left c1, Left c2 ] ->
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


getCellVal : Model -> Int -> Int -> CellModel
getCellVal model i j =
  let
    r =
      withDefault Array.empty <| Array.get i model.values
  in
    withDefault (Right "") <| Array.get j r



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


cell : Model -> Signal.Address Action -> Int -> Int -> CellModel -> Html
cell model address i j cellVal =
  td
    []
    [ input
        [ value (extractValue model i j cellVal)
        , on "input" targetValue (Signal.message address << UpdateCell i j)
        , onFocus address (Focus i j)
        , onBlur address (Blur i j)
        ]
        []
    ]


row : Model -> Signal.Address Action -> Int -> RowModel -> Html
row model address i rowData =
  tr
    []
    (th [] [ text (toString (i + 1)) ] :: (Array.indexedMap (cell model address i) rowData |> Array.toList))


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


header : Model -> Html
header model =
  let
    r =
      withDefault Array.empty (Array.get 0 model.values)
  in
    tr
      []
      (td [] [] :: ((Array.indexedMap (\i a -> th [] [ text (toLiteral (i + 1)) ]) r) |> Array.toList))


view : Signal.Address Action -> Model -> Html
view address model =
  table
    [ class "table" ]
    [ tbody
        []
        (List.append
          [ header model ]
          (Array.indexedMap (row model address) model.values |> Array.toList)
        )
    ]


model : Model
model =
  { values = (Array.fromList [ Array.fromList (List.repeat 50 (Left 1)), Array.fromList (List.repeat 50 (Left 2)) ])
  , focused = ( -1, -1 )
  }


main : Signal Html
main =
  start { model = model, update = update, view = view }
