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

        list =
            sequence
                { start = "["
                , separator = ","
                , spaces = spaces
                , end = "]"
                , item = value -- Should be parseExpr!
                , trailing = Optional
                }
                |> map (\lst -> SimpleValue (VList lst))

        parseExpr : Parser Expr
        parseExpr =
            oneOf
                [ value
                , list
                ]
    in
    case run parseExpr s of
        Ok expr ->
            expr

        Err _ ->
            VError "could not parse" |> SimpleValue
