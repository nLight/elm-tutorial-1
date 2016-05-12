module Int2Literal exposing (..)

import String
import Char


convert : Int -> String
convert i =
    convert' "" i


convert' : String -> Int -> String
convert' acc i =
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
                convert' name i'
