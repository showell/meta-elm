module MeInt exposing
    ( div
    , init
    , toFloat
    , toInt
    )

import MeRunTime
    exposing
        ( computeV
        )
import MeType exposing (..)


fMap2 : (Int -> Int -> Int) -> String -> Expr
fMap2 rawF name =
    let
        f : FVV
        f c expr1 expr2 =
            case ( computeV c expr1, computeV c expr2 ) of
                ( VInt a, VInt b ) ->
                    VInt (rawF a b)

                ( VError s, _ ) ->
                    VError (s ++ " (first arg)")

                ( _, VError s ) ->
                    VError (s ++ " (second arg)")

                _ ->
                    VError "need ints here"
    in
    FunctionVV name f


toFloat : Expr
toFloat =
    let
        f : FV
        f c expr =
            case computeV c expr of
                VInt a ->
                    VFloat (Basics.toFloat a)

                _ ->
                    VError "need int in toFloat"
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


toInt : V -> Result String Int
toInt v =
    case v of
        VInt n ->
            Ok n

        _ ->
            Err "not an int"
