module MeBasics exposing (compare)

{-| wraps Basics


# Comparison

@docs compare

-}

import MeApply
import MeType exposing (Expr(..), FV, V(..))


{-| wraps compare


-}
compare : Expr
compare =
    let
        compare0 : FV
        compare0 =
            MeApply.numberFV compare1Int compare1Float

        compare1Int : Int -> FV
        compare1Int =
            \n1 ->
                MeApply.int <|
                    \_ n2 ->
                        Basics.compare n1 n2
                            |> VOrder

        compare1Float : Float -> FV
        compare1Float =
            \n1 ->
                MeApply.float <|
                    \_ n2 ->
                        Basics.compare n1 n2
                            |> VOrder
    in
    NamedFunc "Basics.compare" compare0
