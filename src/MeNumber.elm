module MeNumber exposing (plus, minus, mult)

{-| wrappers for numbers

@docs plus, minus, mult

-}

import MeRunTime
    exposing
        ( error
        , getValue
        )
import MeType exposing (..)


type alias II =
    Int -> Int -> Int


type alias IF =
    Int -> Float -> Float


type alias FI =
    Float -> Int -> Float


type alias FF =
    Float -> Float -> Float


type alias BinaryImpl =
    { fII : II
    , fIF : IF
    , fFI : FI
    , fFF : FF
    }


binOp : BinaryImpl -> String -> Expr
binOp impl name =
    let
        f0 : FV
        f0 =
            \c expr1 ->
                case getValue c expr1 of
                    VInt a ->
                        a
                            |> f1Int
                            |> ComputedFunc

                    VFloat a ->
                        a
                            |> f1Float
                            |> ComputedFunc

                    _ ->
                        error "not a number"

        f1Int : Int -> FV
        f1Int =
            \a ->
                \c expr2 ->
                    case getValue c expr2 of
                        VInt b ->
                            impl.fII a b
                                |> VInt
                                |> ComputedValue

                        VFloat b ->
                            impl.fIF a b
                                |> VFloat
                                |> ComputedValue

                        _ ->
                            error "not a number"

        f1Float : Float -> FV
        f1Float =
            \a ->
                \c expr2 ->
                    case getValue c expr2 of
                        VInt b ->
                            impl.fFI a b
                                |> VFloat
                                |> ComputedValue

                        VFloat b ->
                            impl.fFF a b
                                |> VFloat
                                |> ComputedValue

                        _ ->
                            error "not a number"
    in
    OpFunc ("(" ++ name ++ ")") f0 name


{-| wraps `+` for numbers
-}
plus : Expr
plus =
    let
        fII : II
        fII a b =
            a + b

        fIF : IF
        fIF a b =
            toFloat a + b

        fFI : FI
        fFI a b =
            a + toFloat b

        fFF : FF
        fFF a b =
            a + b

        impl =
            { fII = fII
            , fIF = fIF
            , fFI = fFI
            , fFF = fFF
            }
    in
    binOp impl "+"


{-| wraps `-` for numbers
-}
minus : Expr
minus =
    let
        fII : II
        fII a b =
            a - b

        fIF : IF
        fIF a b =
            toFloat a - b

        fFI : FI
        fFI a b =
            a - toFloat b

        fFF : FF
        fFF a b =
            a - b

        impl =
            { fII = fII
            , fIF = fIF
            , fFI = fFI
            , fFF = fFF
            }
    in
    binOp impl "-"


{-| wraps `*` for numbers
-}
mult : Expr
mult =
    let
        fII : II
        fII a b =
            a * b

        fIF : IF
        fIF a b =
            toFloat a * b

        fFI : FI
        fFI a b =
            a * toFloat b

        fFF : FF
        fFF a b =
            a * b

        impl =
            { fII = fII
            , fIF = fIF
            , fFI = fFI
            , fFF = fFF
            }
    in
    binOp impl "*"
