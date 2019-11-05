module MeInt exposing
    ( div
    , eq
    , init
    , toFloat
    , toInt
    )

import MeRunTime
    exposing
        ( error
        , getFinalValue
        , getValue
        )
import MeType exposing (..)


fMap2 : (Int -> Int -> Int) -> String -> Expr
fMap2 rawF name =
    let
        f : FVV
        f c expr1 expr2 =
            case ( getValue c expr1, getValue c expr2 ) of
                ( VInt a, VInt b ) ->
                    VInt (rawF a b)
                        |> ComputedValue

                ( VError s, _ ) ->
                    error (s ++ " (first arg)")

                ( _, VError s ) ->
                    error (s ++ " (second arg)")

                _ ->
                    error "need ints here"
    in
    FunctionVV name f


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
    FunctionV "toFloat" f


div : Expr
div =
    let
        f : Int -> Int -> Int
        f a b =
            a // b
    in
    fMap2 f "(//)"


init : Int -> Expr
init num =
    num
        |> VInt
        |> SimpleValue


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


toInt : Expr -> Result String Int
toInt vExpr =
    case getFinalValue vExpr of
        VInt n ->
            Ok n

        _ ->
            Err "not an int"
