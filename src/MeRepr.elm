module MeRepr exposing (fromExpr)

{-| convert Expr to String

@docs fromExpr

-}

import MeRunTime
import MeType
    exposing
        ( Expr(..)
        , V(..)
        )


{-| convert Expr to String
-}
fromExpr : Expr -> String
fromExpr expr =
    case MeRunTime.getFinalValue expr of
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
