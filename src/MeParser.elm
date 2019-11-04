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


parseList item =
    sequence
        { start = "["
        , separator = ","
        , spaces = spaces
        , end = "]"
        , item = item
        , trailing = Optional
        }
        |> map (\lst -> SimpleValue (VList lst))


parseExpr =
    oneOf
        [ parseValue
        , Parser.lazy (\_ -> parseList parseExpr) -- should be parseExpr (not value)
        ]


parseValue =
    oneOf
        [ int |> map (\n -> SimpleValue (VInt n))
        , float |> map (\n -> SimpleValue (VFloat n))
        ]


toExpr : String -> Expr
toExpr s =
    case run parseExpr s of
        Ok expr ->
            expr

        Err _ ->
            VError "could not parse" |> SimpleValue
