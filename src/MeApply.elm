module MeApply exposing
    ( int, float, list, listExpr, tuple
    , exprFV, intFV, fvFV, numberFV
    )

{-| helpers for wrappers


# function -> value

@docs int, float, list, listExpr, tuple


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
        ( Context
        , Expr(..)
        , FV
        , V(..)
        )


{-| creates FV for f(float)
-}
float : (Context -> Float -> V) -> FV
float =
    \f ->
        \c expr ->
            case getValue c expr of
                VFloat n ->
                    n
                        |> f c
                        |> ComputedValue

                _ ->
                    error "not a float"


{-| creates FV for f(int)
-}
int : (Context -> Int -> V) -> FV
int =
    \f ->
        \c expr ->
            case getValue c expr of
                VInt n ->
                    n
                        |> f c
                        |> ComputedValue

                _ ->
                    error "not a int"


{-| creates Expr for f(list)
-}
listExpr : (Context -> List Expr -> Expr) -> FV
listExpr =
    \f ->
        \c lstExpr ->
            case getValue c lstExpr of
                VList lst ->
                    lst
                        |> f c

                _ ->
                    error "not a list"


{-| creates FV for f(list)
-}
list : (Context -> List Expr -> V) -> FV
list =
    \fv ->
        listExpr <|
            \c lst ->
                lst
                    |> fv c
                    |> ComputedValue


{-| creates FV for f(tuple)
-}
tuple : (( Expr, Expr ) -> Expr) -> FV
tuple =
    \f ->
        \c expr ->
            case getValue c expr of
                VTuple tup ->
                    tup
                        |> f
                        |> compute c

                _ ->
                    error "not a tuple"


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
