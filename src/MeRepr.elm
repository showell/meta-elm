module MeRepr exposing
    ( fromExpr
    , fromList, fromTuple, fromV
    )

{-| convert Expr to String

@docs fromExpr


# Helpers

@docs fromList, fromTuple, fromV

-}

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
    case expr of
        ComputedValue v ->
            fromV v fromExpr

        SimpleValue v ->
            fromV v fromExpr

        _ ->
            "????"


{-| convert Value to String

    pass in helper to convert subexpressions

    (usually you just wanna call fromExpr)

-}
fromV : V -> (Expr -> String) -> String
fromV val helper =
    case val of
        VList lst ->
            lst
                |> List.map helper
                |> fromList

        VTuple ( a, b ) ->
            ( a |> helper
            , b |> helper
            )
                |> fromTuple

        VMaybe m ->
            case m of
                Just v ->
                    "Just " ++ (v |> helper)

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
