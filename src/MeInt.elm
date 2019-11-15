module MeInt exposing
    ( init, toInt
    , eq, toFloat, modBy
    )

{-| helper module for Int types


# conversion

@docs init, toInt


# expression

@docs eq, toFloat, modBy

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


{-| wraps modBy
-}
modBy : Expr
modBy =
    let
        modBy0 : FV
        modBy0 =
            \c expr1 ->
                case getValue c expr1 of
                    VInt n1 ->
                        n1
                            |> modBy1
                            |> ComputedFunc

                    _ ->
                        error "need int for first arg"

        modBy1 : Int -> FV
        modBy1 =
            \n1 ->
                \c expr2 ->
                    case getValue c expr2 of
                        VInt n2 ->
                            Basics.modBy n1 n2
                                |> VInt
                                |> ComputedValue

                        _ ->
                            error "need int for second arg"
    in
    NamedFunc "modBy" modBy0


{-| wraps `==` for ints
-}
eq : Expr
eq =
    let
        eq0 : FV
        eq0 =
            \c expr1 ->
                case getValue c expr1 of
                    VInt n1 ->
                        n1
                            |> eq1
                            |> ComputedFunc

                    _ ->
                        error "need int for first arg"

        eq1 : Int -> FV
        eq1 =
            \n1 ->
                \c expr2 ->
                    case getValue c expr2 of
                        VInt n2 ->
                            (n1 == n2)
                                |> VBool
                                |> ComputedValue

                        _ ->
                            error "need int for second arg"
    in
    OpFunc "eq" eq0 "=="


{-| convert Expr to raw Int (if types match)
-}
toInt : Expr -> Result String Int
toInt vExpr =
    case getFinalValue vExpr of
        VInt n ->
            Ok n

        _ ->
            Err "not an int"
