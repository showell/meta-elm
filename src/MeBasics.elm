module MeBasics exposing (compare)

{-| wraps Basics


# Comparison

@docs compare

-}

import MeRunTime
    exposing
        ( error
        , getValue
        )
import MeType exposing (Expr(..), FV, V(..))



{--wraps compare
--}


compare : Expr
compare =
    let
        compare0 : FV
        compare0 =
            \c expr1 ->
                case getValue c expr1 of
                    VInt n ->
                        n
                            |> compare1Int
                            |> ComputedFunc

                    VFloat n ->
                        n
                            |> compare1Float
                            |> ComputedFunc

                    _ ->
                        error "not a supported comparable"

        compare1Int : Int -> FV
        compare1Int =
            \n1 ->
                \c expr2 ->
                    case getValue c expr2 of
                        VInt n2 ->
                            Basics.compare n1 n2
                                |> VOrder
                                |> ComputedValue

                        _ ->
                            error "cannot be compared to int"

        compare1Float : Float -> FV
        compare1Float =
            \n1 ->
                \c expr2 ->
                    case getValue c expr2 of
                        VFloat n2 ->
                            Basics.compare n1 n2
                                |> VOrder
                                |> ComputedValue

                        _ ->
                            error "cannot be compared to float"
    in
    NamedFunc "Basics.compare" compare0
