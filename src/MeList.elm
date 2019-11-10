module MeList exposing
    ( initInts, initFloats
    , toList, toListInts
    , cons, plus
    , map, indexedMap, sortByInt, sortByFloat, sortBy, foldl
    , range, singleton, sortInt, sortFloat
    )

{-| wrap List


# conversion (in)

@docs initInts, initFloats


# conversion (out)

@docs toList, toListInts


# operators

@docs cons, plus


# wrappers for functions taking functions

@docs map, indexedMap, sortByInt, sortByFloat, sortBy, foldl


# simple wrappers

@docs range, singleton, sortInt, sortFloat

-}

import MeFloat
import MeInt
import MeRunTime exposing (..)
import MeType exposing (..)


{-| wraps List.indexedMap
-}
indexedMap : Expr
indexedMap =
    let
        indexedMap2 : List Expr -> (Expr -> Expr -> Expr) -> V
        indexedMap2 lst mapper =
            let
                wrapped_mapper idx item =
                    let
                        idxExpr =
                            ComputedValue (VInt idx)
                    in
                    mapper idxExpr item
            in
            lst
                |> List.indexedMap wrapped_mapper
                |> VList

        indexedMap1 : FVV -> FV
        indexedMap1 mapper =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        indexedMap2 lst (mapper c)
                            |> ComputedValue

                    VError s ->
                        error ("bad list in indexedMap: " ++ s)

                    _ ->
                        error "need list in indexedMap"

        indexedMap0 : FV
        indexedMap0 c mapperExpr =
            indexedMap1 (getFuncVV c mapperExpr)
                |> ComputedFunc
    in
    NamedFunc "List.indexedMap" indexedMap0


{-| wraps List.sort for floats
-}
sortFloat : Expr
sortFloat =
    sort MeFloat.toFloat


{-| wraps List.sort for ints
-}
sortInt : Expr
sortInt =
    sort MeInt.toInt


transformSort : FV -> (Expr -> Result String comparable) -> Context -> List Expr -> Expr
transformSort ord unbox c exprLst =
    let
        listOfTups : List Expr -> Result String (List ( comparable, Expr ))
        listOfTups lst =
            case lst of
                [] ->
                    Ok []

                head :: rest ->
                    case unbox (ord c head) of
                        Ok h ->
                            case listOfTups rest of
                                Ok others ->
                                    ( h, head )
                                        :: others
                                        |> Ok

                                Err s ->
                                    Err s

                        Err s ->
                            Err s
    in
    case listOfTups exprLst of
        Ok tups ->
            tups
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
                |> VList
                |> ComputedValue

        Err s ->
            error s


sort : (Expr -> Result String comparable) -> Expr
sort unbox =
    let
        ord : FV
        ord c expr =
            compute c expr

        sort0 : FV
        sort0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        transformSort ord unbox c lst

                    _ ->
                        error "sort wants a list"
    in
    NamedFunc "List.sort" sort0


{-| wraps List.sortBy for ints
-}
sortByInt : Expr
sortByInt =
    sortBy MeInt.toInt


{-| wraps List.sortBy for floats
-}
sortByFloat : Expr
sortByFloat =
    sortBy MeFloat.toFloat


{-| wraps List.sortBy for general types
(but needs an unboxer like `MeInt.toInt`)

Often you can just use `sortByInt` or `sortByFloat` for convenience.

-}
sortBy : (Expr -> Result String comparable1) -> Expr
sortBy unbox =
    let
        sortBy1 : FV -> FV
        sortBy1 ord =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        transformSort ord unbox c lst

                    VError s ->
                        error ("bad list in sortBy: " ++ s)

                    _ ->
                        error "need list in sortBy"

        sortBy0 : FV
        sortBy0 c ordExpr =
            let
                ord =
                    getFuncV c ordExpr
            in
            sortBy1 ord
                |> ComputedFunc
    in
    NamedFunc "List.sortBy" sortBy0


{-| wraps List.foldl
-}
foldl : Expr
foldl =
    let
        foldl2 : FVV -> Expr -> FV
        foldl2 accum startVal =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        List.foldl (accum c) startVal lst

                    VError s ->
                        error ("bad list in foldl: " ++ s)

                    _ ->
                        error "need list in foldl"

        foldl1 : FVV -> FV
        foldl1 accum =
            \c startVal ->
                foldl2 accum (compute c startVal)
                    |> ComputedFunc

        fold0 : FV
        fold0 c accumExpr =
            foldl1 (getFuncVV c accumExpr)
                |> ComputedFunc
    in
    NamedFunc "List.foldl" fold0


{-| wraps List.map
-}
map : Expr
map =
    let
        happyPath : Context -> List Expr -> FV -> Expr
        happyPath c lst mapper =
            lst
                |> List.map (mapper c)
                |> VList
                |> ComputedValue

        map1 : FV -> FV
        map1 mapper =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        happyPath c lst mapper

                    VError s ->
                        error ("bad list in map: " ++ s)

                    _ ->
                        error "need list in map"

        map0 : FV
        map0 c mapperExpr =
            let
                mapper =
                    getFuncV c mapperExpr
            in
            map1 mapper
                |> ComputedFunc
    in
    NamedFunc "List.map" map0


{-| wraps List.range
-}
range : Expr
range =
    let
        range1 : Int -> (Context -> Expr -> Expr)
        range1 lo =
            \c hiExpr ->
                case getValue c hiExpr of
                    VInt hi ->
                        List.range lo hi
                            |> List.map VInt
                            |> List.map ComputedValue
                            |> VList
                            |> ComputedValue

                    _ ->
                        error "high value to range must be integer"

        range0 : Context -> Expr -> Expr
        range0 c loExpr =
            case getValue c loExpr of
                VInt lo ->
                    range1 lo
                        |> ComputedFunc

                _ ->
                    error "low value to range must be integer"
    in
    NamedFunc "List.range" range0


{-| wraps List.singleton
-}
singleton : Expr
singleton =
    let
        f : FV
        f c expr =
            [ ComputedValue (getValue c expr) ]
                |> VList
                |> ComputedValue
    in
    NamedFunc "List.singleton" f


{-| wraps `::`
-}
cons : Expr
cons =
    let
        f : FVV
        f c expr1 expr2 =
            case ( getValue c expr1, getValue c expr2 ) of
                ( VError s, _ ) ->
                    error ("bad arg to :: - " ++ s)

                ( h, VList lst ) ->
                    VList (ComputedValue h :: lst)
                        |> ComputedValue

                ( _, _ ) ->
                    error "need list to cons to"
    in
    BinOp "::" f


{-| wraps '++' (for lists)
-}
plus : Expr
plus =
    let
        f : FVV
        f c expr1 expr2 =
            case ( getValue c expr1, getValue c expr2 ) of
                ( VList lst1, VList lst2 ) ->
                    VList (lst1 ++ lst2)
                        |> ComputedValue

                ( _, _ ) ->
                    error "need lists in ++"
    in
    BinOp "++" f


{-| convert list of ints to an Expr
-}
initInts : List Int -> Expr
initInts nums =
    nums
        |> List.map MeInt.init
        |> VList
        |> SimpleValue


{-| convert list of floats to an Expr
-}
initFloats : List Float -> Expr
initFloats nums =
    nums
        |> List.map MeFloat.init
        |> VList
        |> SimpleValue


{-| convert wrapped list to List

You must pass in a converter for the elements. Example:

        expr
            |> MeRunTime.getFinalValue
            |> toList MeInt.toInt
            |> Result.withDefault []

-}
toList : (Expr -> Result String a) -> V -> Result String (List a)
toList convert vList =
    let
        f lst =
            case lst of
                [] ->
                    Ok []

                head :: rest ->
                    case ( convert head, f rest ) of
                        ( Ok h, Ok r ) ->
                            Ok (h :: r)

                        ( Err s, _ ) ->
                            Err s

                        ( _, Err s ) ->
                            Err s
    in
    case vList of
        VList lst ->
            f lst

        _ ->
            Err "not a list"


{-| convert wrapped list of ints to List Int
-}
toListInts : V -> Result String (List Int)
toListInts vList =
    toList MeInt.toInt vList
