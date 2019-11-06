module MeParser exposing (toExpr)

import DParser
    exposing
        ( D(..)
        )
import MeType
    exposing
        ( Expr(..)
        , V(..)
        )


toExprHelper : D -> Expr
toExprHelper d =
    case d of
        DInt n ->
            VInt n
                |> SimpleValue

        DFloat n ->
            VFloat n
                |> SimpleValue

        DList lst ->
            lst
                |> List.map toExprHelper
                |> VList
                |> SimpleValue


toExpr : String -> Expr
toExpr text =
    case DParser.parse text of
        Ok d ->
            toExprHelper d

        Err s ->
            VError s
                |> SimpleValue
