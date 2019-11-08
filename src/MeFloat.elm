module MeFloat exposing (init, toFloat)

{-| helper module for Float types

@docs init, toFloat

-}

import MeRunTime
    exposing
        ( getFinalValue
        )
import MeType exposing (..)


{-| turn raw Float into Exp
-}
init : Float -> Expr
init num =
    num
        |> VFloat
        |> SimpleValue


{-| convert Expr to raw Float (if types match)
-}
toFloat : Expr -> Result String Float
toFloat vExpr =
    case getFinalValue vExpr of
        VFloat n ->
            Ok n

        _ ->
            Err "not an int"
