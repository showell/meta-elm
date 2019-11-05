module MeFloat exposing
    ( init
    , toFloat
    )

import MeRunTime
    exposing
        ( getFinalValue
        )
import MeType exposing (..)


init : Float -> Expr
init num =
    num
        |> VFloat
        |> SimpleValue


toFloat : Expr -> Result String Float
toFloat vExpr =
    case getFinalValue vExpr of
        VFloat n ->
            Ok n

        _ ->
            Err "not an int"
