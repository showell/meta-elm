module MeRepr exposing
    ( fromExpr
    , fromVal
    )

import MeRunTime
import MeType
    exposing
        ( Expr(..)
        , V(..)
        )


fromVal : V -> String
fromVal v =
    fromExpr (ComputedValue v)


fromExpr : Expr -> String
fromExpr expr =
    case MeRunTime.getFinalValue expr of
        Ok v ->
            case v of
                VList lst ->
                    let
                        items =
                            lst
                                |> List.map fromExpr
                    in
                    "["
                        ++ String.join ", " items
                        ++ "]"
                        |> String.replace "], " "]\n,"

                VTuple ( a, b ) ->
                    "("
                        ++ fromExpr a
                        ++ ", "
                        ++ fromExpr b
                        ++ ")"

                VBool b ->
                    if b then
                        "true"

                    else
                        "false"

                VInt n ->
                    String.fromInt n

                VFloat n ->
                    String.fromFloat n

                VError s ->
                    "error: " ++ s

        Err s ->
            "error: " ++ s
