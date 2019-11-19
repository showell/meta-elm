module MeTuple exposing (first, second, pair)

{-| wraps Tuple

@docs first, second, pair

-}

import MeApply
import MeRunTime exposing (compute, error, getValue)
import MeType exposing (..)


{-| wraps Tuple.first
-}
first : Expr
first =
    Tuple.first
        |> MeApply.tuple
        |> NamedFunc "tuple.first"


{-| wraps Tuple.second
-}
second : Expr
second =
    Tuple.second
        |> MeApply.tuple
        |> NamedFunc "Tuple.second"


{-| wraps Tuple.pair
-}
pair : Expr
pair =
    let
        pair0 : FV
        pair0 =
            MeApply.exprFV pair1

        pair1 : Expr -> FV
        pair1 left =
            \c rightExpr ->
                rightExpr
                    |> compute c
                    |> Tuple.pair left
                    |> VTuple
                    |> ComputedValue
    in
    NamedFunc "Tuple.pair" pair0
