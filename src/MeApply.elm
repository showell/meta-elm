module MeApply exposing
    ( list
    , exprFV, intFV, fvFV, numberFV
    )

{-| helpers for wrappers


# function -> value

@docs list


# function -> function

@docs exprFV, intFV, fvFV, numberFV

-}

import MeRunTime
    exposing
        ( compute
        , error
        , getFuncV
        , getValue
        )
import MeType
    exposing
        ( Expr(..)
        , FV
        , V(..)
        )


{-| creates FV for f(list)
-}
list : (List Expr -> V) -> FV
list =
    \f ->
        \c lstExpr ->
            case getValue c lstExpr of
                VList lst ->
                    lst
                        |> f
                        |> ComputedValue

                _ ->
                    error "not a list"


{-| create FV for f(expr, ...)
-}
exprFV : (Expr -> FV) -> FV
exprFV =
    \f ->
        \c expr ->
            expr
                |> compute c
                |> f
                |> ComputedFunc


{-| create FV for f(a -> a, ...)
-}
fvFV : (FV -> FV) -> FV
fvFV =
    \f ->
        \c expr ->
            expr
                |> getFuncV c
                |> f
                |> ComputedFunc


{-| create FV for f(Int, ...)
-}
intFV : (Int -> FV) -> FV
intFV =
    \f ->
        \c expr ->
            case getValue c expr of
                VInt n ->
                    n
                        |> f
                        |> ComputedFunc

                _ ->
                    error "not an Int"


{-| create FV for f(number, ...)
-}
numberFV : (Int -> FV) -> (Float -> FV) -> FV
numberFV =
    \fInt fFloat ->
        \c expr ->
            case getValue c expr of
                VInt n ->
                    n
                        |> fInt
                        |> ComputedFunc

                VFloat n ->
                    n
                        |> fFloat
                        |> ComputedFunc

                _ ->
                    error "not a number"
