module MeParser exposing (toExpr)

{-| parse JSON -> Expr

@docs toExpr

-}

import Json.Decode
    exposing
        ( decodeString
        , errorToString
        )
import Json.Decode.Generic
    exposing
        ( Json(..)
        , json
        )
import MeType
    exposing
        ( Expr(..)
        , V(..)
        )


toExprHelper : Json -> Expr
toExprHelper d =
    case d of
        JInt n ->
            n
                |> VInt
                |> SimpleValue

        JFloat n ->
            n
                |> VFloat
                |> SimpleValue

        JArr lst ->
            lst
                |> List.map toExprHelper
                |> VList
                |> SimpleValue

        _ ->
            "unsupported type"
                |> VError
                |> SimpleValue


{-| convert JSON to Expr

    (note that we don't have a way to read in tuples)

-}
toExpr : String -> Expr
toExpr text =
    case decodeString json text of
        Ok d ->
            toExprHelper d

        Err error ->
            error
                |> errorToString
                |> VError
                |> SimpleValue
