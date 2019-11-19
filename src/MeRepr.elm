module MeRepr exposing
    ( fromExpr
    , fromList, fromTuple
    )

{-| convert Expr to String

@docs fromExpr


# Helpers

@docs fromList, fromTuple

-}

import MeRunTime
import MeType
    exposing
        ( Expr(..)
        , V(..)
        )


{-| emit a list of strings (i.e. surround with brackets, add commas)
-}
fromList : List String -> String
fromList items =
    "[ "
        ++ String.join ", " items
        ++ " ]"
        |> String.replace "], " "]\n, "
        |> String.replace "), " ")\n, "


{-| emit a tuple of strings (i.e. surround with parens, add commas)
-}
fromTuple : ( String, String ) -> String
fromTuple ( a, b ) =
    "( "
        ++ a
        ++ ", "
        ++ b
        ++ " )"


{-| convert Expr to String
-}
fromExpr : Expr -> String
fromExpr expr =
    case MeRunTime.getFinalValue expr of
        VList lst ->
            lst
                |> List.map fromExpr
                |> fromList

        VTuple ( a, b ) ->
            ( a |> fromExpr
            , b |> fromExpr
            )
                |> fromTuple

        VMaybe m ->
            case m of
                Just v ->
                    "Just " ++ (v |> fromExpr)

                Nothing ->
                    "Nothing"

        VBool b ->
            if b then
                "true"

            else
                "false"

        VOrder ord ->
            case ord of
                EQ ->
                    "EQ"

                LT ->
                    "LT"

                GT ->
                    "GT"

        VInt n ->
            String.fromInt n

        VFloat n ->
            String.fromFloat n

        VError s ->
            "error: " ++ s
