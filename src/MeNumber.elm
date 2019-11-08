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
        f : FVV
        f c expr1 expr2 =
            case ( getValue c expr1, getValue c expr2 ) of
                ( VInt a, VInt b ) ->
                    VInt (impl.fII a b)
                        |> ComputedValue

                ( VInt a, VFloat b ) ->
                    VFloat (impl.fIF a b)
                        |> ComputedValue

                ( VFloat a, VInt b ) ->
                    VFloat (impl.fFI a b)
                        |> ComputedValue

                ( VFloat a, VFloat b ) ->
                    VFloat (impl.fFF a b)
                        |> ComputedValue

                ( VError s, _ ) ->
                    error (s ++ " (first arg)")

                ( _, VError s ) ->
                    error (s ++ " (second arg)")

                _ ->
                    error "need numbers here"
    in
    BinOp name f


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
