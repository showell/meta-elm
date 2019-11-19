module MeWrapper exposing (allWrappers, viewWrappers)

{-| Advanced:

This returns a Dict mapping names like "List.map"
to the associated wrapper functions (e.g. `MeList.map`).

Generally, you do not want to use this module unless
you're building something like a REPL. Instead just
directly use things like `MeList.map`. When you use
`allWrappers` it brings in all the associated code.

@docs allWrappers, viewWrappers

-}

import Dict exposing (Dict)
import Html exposing (Html)
import MeBasics
import MeInt
import MeList
import MeTuple
import MeType exposing (Expr(..))


{-| Dict mapping names like
"List.name" to associated meta-elm wrapper expression
-}
allWrappers : Dict String Expr
allWrappers =
    allFunctions
        |> List.map (\expr -> ( funcName expr, expr ))
        |> Dict.fromList


allFunctions : List Expr
allFunctions =
    [ MeBasics.compare
    , MeList.all
    , MeList.any
    , MeList.append
    , MeList.concat
    , MeList.concatMap
    , MeList.cons
    , MeList.drop
    , MeList.filter
    , MeList.filterMap
    , MeList.foldl
    , MeList.foldr
    , MeList.head
    , MeList.indexedMap
    , MeList.intersperse
    , MeList.isEmpty
    , MeList.length
    , MeList.map
    , MeList.map2
    , MeList.map3
    , MeList.map4
    , MeList.map5
    , MeList.maximum
    , MeList.member
    , MeList.minimum
    , MeList.partition
    , MeList.product
    , MeList.range
    , MeList.repeat
    , MeList.reverse
    , MeList.singleton
    , MeList.sort
    , MeList.sortBy
    , MeList.sum
    , MeList.tail
    , MeList.take
    , MeList.unzip
    , MeTuple.first
    , MeTuple.pair
    , MeTuple.second
    , MeInt.toFloat
    , MeInt.modBy
    ]


funcName : Expr -> String
funcName expr =
    case expr of
        NamedFunc name _ ->
            name

        OpFunc name _ _ ->
            name

        Var name _ ->
            name

        _ ->
            "?"


{-| vanilla HTML of supported wrapper methods

You will generally only display this if you are using
allWrappers. If you want to display these more nicely, you
can read the source.

-}
viewWrappers : List (Html msg)
viewWrappers =
    allWrappers
        |> Dict.keys
        |> List.map Html.text
        |> List.map List.singleton
        |> List.map (Html.li [])
        |> Html.ul []
        |> List.singleton
