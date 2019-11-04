module MeNumber exposing (plus)

import MeRunTime
    exposing
        ( computeV
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
            case ( computeV c expr1, computeV c expr2 ) of
                ( VInt a, VInt b ) ->
                    VInt (impl.fII a b)

                ( VInt a, VFloat b ) ->
                    VFloat (impl.fIF a b)

                ( VFloat a, VInt b ) ->
                    VFloat (impl.fFI a b)

                ( VFloat a, VFloat b ) ->
                    VFloat (impl.fFF a b)

                ( VError s, _ ) ->
                    VError (s ++ " (first arg)")

                ( _, VError s ) ->
                    VError (s ++ " (second arg)")

                _ ->
                    VError "need numbers here"
    in
    BinOp name f


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
