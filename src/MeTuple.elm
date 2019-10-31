module MeTuple exposing
    ( first
    , pair
    , second
    )

import MeRunTime exposing (computeV)
import MeType exposing (..)


fFrom : String -> (( Expr, Expr ) -> Expr) -> Expr
fFrom name rawF =
    let
        f : FV
        f c expr =
            case computeV c expr of
                VTuple tup ->
                    computeV c (rawF tup)

                _ ->
                    VError "not a tuple"
    in
    FunctionV name f


first : Expr
first =
    fFrom "Tuple.first" Tuple.first


second : Expr
second =
    fFrom "Tuple.second" Tuple.second


pair : Expr
pair =
    let
        f : FVV
        f _ expr1 expr2 =
            VTuple ( expr1, expr2 )
    in
    FunctionVV "Tuple.pair" f
