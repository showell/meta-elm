module MeList exposing
    ( initInts, initFloats
    , toList, toListInts
    , cons, plus
    , map, indexedMap, sortBy, foldl, foldr, filter
    , range, repeat, singleton, sort
    )

{-| wrap List


# conversion (in)

@docs initInts, initFloats


# conversion (out)

@docs toList, toListInts


# operators

@docs cons, plus


# wrappers for functions taking functions

@docs map, indexedMap, sortBy, foldl, foldr, filter


# simple wrappers

@docs range, repeat, singleton, sort

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


transformSort : FV -> Context -> List Expr -> Expr
transformSort ord c lst =
    let
        comp item1 item2 =
            compare
                (Tuple.first item1)
                (Tuple.first item2)
    in
    lst
        |> List.map (\item -> ( ord c item, item ))
        |> List.sortWith comp
        |> List.map Tuple.second
        |> VList
        |> ComputedValue


{-| wraps sort
-}
sort : Expr
sort =
    let
        ord : FV
        ord c expr =
            compute c expr

        sort0 : FV
        sort0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        transformSort ord c lst

                    _ ->
                        error "sort wants a list"
    in
    NamedFunc "List.sort" sort0


{-| wraps List.sortBy
-}
sortBy : Expr
sortBy =
    let
        sortBy1 : FV -> FV
        sortBy1 ord =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        transformSort ord c lst

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

        foldl0 : FV
        foldl0 c accumExpr =
            foldl1 (getFuncVV c accumExpr)
                |> ComputedFunc
    in
    NamedFunc "List.foldl" foldl0


{-| wraps List.foldr
-}
foldr : Expr
foldr =
    let
        foldr2 : FVV -> Expr -> FV
        foldr2 accum startVal =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        List.foldr (accum c) startVal lst

                    VError s ->
                        error ("bad list in foldr: " ++ s)

                    _ ->
                        error "need list in foldr"

        foldr1 : FVV -> FV
        foldr1 accum =
            \c startVal ->
                foldr2 accum (compute c startVal)
                    |> ComputedFunc

        foldr0 : FV
        foldr0 c accumExpr =
            foldr1 (getFuncVV c accumExpr)
                |> ComputedFunc
    in
    NamedFunc "List.foldr" foldr0


{-| wraps List.filter
-}
filter : Expr
filter =
    let
        filter0 : FV
        filter0 =
            \c predExpr ->
                let
                    pred =
                        getFuncV c predExpr
                in
                filter1 pred
                    |> ComputedFunc

        filter1 : FV -> FV
        filter1 =
            \pred ->
                \c lstExpr ->
                    let
                        filterer expr =
                            case getValue c (pred c expr) of
                                VBool b ->
                                    b

                                _ ->
                                    False
                    in
                    case getValue c lstExpr of
                        VList lst ->
                            lst
                                |> List.filter filterer
                                |> VList
                                |> ComputedValue

                        VError s ->
                            error ("bad list in filter: " ++ s)

                        _ ->
                            error "need list in filter"
    in
    NamedFunc "List.filter" filter0


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


{-| wraps List.repeat
-}
repeat : Expr
repeat =
    let
        repeat1 : Int -> (Context -> Expr -> Expr)
        repeat1 n =
            \c vExpr ->
                vExpr
                    |> List.repeat n
                    |> VList
                    |> ComputedValue

        repeat0 : Context -> Expr -> Expr
        repeat0 =
            \c nExpr ->
                case getValue c nExpr of
                    VInt n ->
                        repeat1 n
                            |> ComputedFunc

                    _ ->
                        error "first arg to repeat must be an integer"
    in
    NamedFunc "List.repeat" repeat0


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


compare : Expr -> Expr -> Order
compare vExpr1 vExpr2 =
    case ( getFinalValue vExpr1, getFinalValue vExpr2 ) of
        ( VInt n1, VInt n2 ) ->
            Basics.compare n1 n2

        ( VFloat n1, VFloat n2 ) ->
            Basics.compare n1 n2

        _ ->
            Basics.EQ
