module MeInt exposing
    ( init, toInt
    , eq, toFloat
    )

{-| helper module for Int types


# conversion

@docs init, toInt


# expression

@docs eq, toFloat

-}

import MeRunTime
    exposing
        ( error
        , getFinalValue
        , getValue
        )
import MeType exposing (..)


{-| wraps Basics.toFloat
-}
toFloat : Expr
toFloat =
    let
        f : FV
        f c expr =
            case getValue c expr of
                VInt a ->
                    VFloat (Basics.toFloat a)
                        |> ComputedValue

                _ ->
                    error "need int in toFloat"
    in
    NamedFunc "toFloat" f


{-| turn raw Int into an Expr
-}
init : Int -> Expr
init num =
    num
        |> VInt
        |> SimpleValue


{-| wraps `==` for ints
-}
eq : Expr
eq =
    let
        f : FVV
        f c expr1 expr2 =
            case ( getValue c expr1, getValue c expr2 ) of
                ( VInt a, VInt b ) ->
                    VBool (a == b)
                        |> ComputedValue

                ( VError s, _ ) ->
                    error (s ++ " (first arg)")

                ( _, VError s ) ->
                    error (s ++ " (second arg)")

                _ ->
                    error "need numbers here"
    in
    BinOp "==" f


{-| convert Expr to raw Int (if types match)
-}
toInt : Expr -> Result String Int
toInt vExpr =
    case getFinalValue vExpr of
        VInt n ->
            Ok n

        _ ->
            Err "not an int"
