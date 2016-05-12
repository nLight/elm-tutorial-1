module Spreadsheet exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, value)
import Html.Events exposing (onInput, onFocus, onBlur, targetValue)
import Html.App as HtmlApp
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


type Msg
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


update : Msg -> Model -> Model
update message model =
    case message of
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


cell : Model -> Coords -> Html Msg
cell model coords =
    let
        cellVal =
            Dict.get (getIndex coords) model.values
    in
        td []
            [ input
                [ value (extractValue model coords cellVal)
                , onInput (UpdateCell (Just coords))
                , onFocus (Focus coords)
                ]
                []
            ]


row : Model -> Int -> Html Msg
row model i =
    let
        sizeArray =
            Array.repeat defaultSize 0

        cells =
            Array.indexedMap (\j el -> cell model ( i, j )) sizeArray |> Array.toList
    in
        tr []
            (th [] [ text (toString (i + 1)) ] :: cells)


header : Model -> Html Msg
header model =
    let
        r =
            Array.repeat defaultSize 0
    in
        tr []
            (td [] [] :: ((Array.indexedMap (\i a -> th [] [ text (toLiteral (i + 1)) ]) r) |> Array.toList))


sheet : Model -> Html Msg
sheet model =
    let
        sheetArray =
            Array.repeat defaultSize 0
    in
        table [ class "table" ]
            [ tbody []
                (List.append [ header model ]
                    (Array.indexedMap (\i el -> row model i) sheetArray |> Array.toList)
                )
            ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ input
            [ value (getFocusedValue model)
            , onInput (UpdateCell model.focused)
            ]
            []
        , sheet model
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


main : Program Never
main =
    HtmlApp.beginnerProgram { model = model, update = update, view = view }
