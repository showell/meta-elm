module MeParser exposing (toExpr)

import MeType
    exposing
        ( Expr(..)
        , V(..)
        )
import Parser
    exposing
        ( Parser
        , Trailing(..)
        , float
        , int
        , lazy
        , map
        , oneOf
        , run
        , sequence
        , spaces
        )


toExpr : String -> Expr
toExpr s =
    let
        {--
            TODO: get lists of lists working
                (lazy seems broken in 0.19, so mutual
                recursion is impossible
        --}
        value =
            oneOf
                [ int |> map (\n -> SimpleValue (VInt n))
                , float |> map (\n -> SimpleValue (VFloat n))
                ]

        list item =
            sequence
                { start = "["
                , separator = ","
                , spaces = spaces
                , end = "]"
                , item = item
                , trailing = Optional
                }
                |> map (\lst -> SimpleValue (VList lst))

        parseExpr : Parser Expr
        parseExpr =
            oneOf
                [ value
                , Parser.lazy (\_ -> list value) -- should be parseExpr (not value)
                ]
    in
    case run parseExpr s of
        Ok expr ->
            expr

        Err _ ->
            VError "could not parse" |> SimpleValue
