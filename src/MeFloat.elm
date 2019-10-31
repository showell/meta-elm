module MeFloat exposing
    ( init
    , toFloat
    )

import MeType exposing (..)


init : Float -> Expr
init num =
    num
        |> VFloat
        |> SimpleValue


toFloat : V -> Result String Float
toFloat v =
    case v of
        VFloat n ->
            Ok n

        _ ->
            Err "not an int"
