module MeTuple exposing (first, second, pair)

{-| wraps Tuple

@docs first, second, pair

-}

import MeApply
import MeRunTime exposing (compute, error, getValue)
import MeType exposing (..)


fFrom : String -> (( Expr, Expr ) -> Expr) -> Expr
fFrom name rawF =
    let
        f : FV
        f c expr =
            case getValue c expr of
                VTuple tup ->
                    compute c (rawF tup)

                _ ->
                    error "not a tuple"
    in
    NamedFunc name f


{-| wraps Tuple.first
-}
first : Expr
first =
    fFrom "Tuple.first" Tuple.first


{-| wraps Tuple.second
-}
second : Expr
second =
    fFrom "Tuple.second" Tuple.second


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
                let
                    right =
                        compute c rightExpr
                in
                VTuple (Tuple.pair left right)
                    |> ComputedValue
    in
    NamedFunc "Tuple.pair" pair0
