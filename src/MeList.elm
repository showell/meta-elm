module MeList exposing
    ( initInts, initFloats, empty
    , toList, toListInts
    , cons, append
    , all, any, concat, concatMap, drop, filter, filterMap, foldl, foldr, head, indexedMap, isEmpty, length, map, map2, map3, maximum, member, minimum, product, range, repeat, reverse, singleton, sort, sortBy, sum, tail, take
    )

{-| wrap List


# conversion (in)

@docs initInts, initFloats, empty


# conversion (out)

@docs toList, toListInts


# operators

@docs cons, append


# wrappers

@docs all, any, concat, concatMap, drop, filter, filterMap, foldl, foldr, head, indexedMap, isEmpty, length, map, map2, map3, maximum, member, minimum, product, range, repeat, reverse, singleton, sort, sortBy, sum, tail, take

-}

import MeFloat
import MeInt
import MeNumber
import MeRunTime exposing (..)
import MeType exposing (..)


{-| wraps List.indexedMap
-}
indexedMap : Expr
indexedMap =
    let
        indexedMap0 : FV
        indexedMap0 =
            \c mapperExpr ->
                mapperExpr
                    |> getFuncVV c
                    |> indexedMap1
                    |> ComputedFunc

        indexedMap1 : FVV -> FV
        indexedMap1 =
            \mapper ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            indexedMap2 lst (mapper c)
                                |> ComputedValue

                        VError s ->
                            error ("bad list in indexedMap: " ++ s)

                        _ ->
                            error "need list in indexedMap"

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


{-| wraps minimum
-}
minimum : Expr
minimum =
    let
        minimum0 : FV
        minimum0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        case lst of
                            x :: xs ->
                                Just (List.foldl min x xs)
                                    |> VMaybe
                                    |> ComputedValue

                            _ ->
                                Nothing
                                    |> VMaybe
                                    |> ComputedValue

                    _ ->
                        error "minimum wants a list"
    in
    NamedFunc "List.minimum" minimum0


{-| wraps maximum
-}
maximum : Expr
maximum =
    let
        maximum0 : FV
        maximum0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        case lst of
                            x :: xs ->
                                Just (List.foldl max x xs)
                                    |> VMaybe
                                    |> ComputedValue

                            _ ->
                                Nothing
                                    |> VMaybe
                                    |> ComputedValue

                    _ ->
                        error "maximum wants a list"
    in
    NamedFunc "List.maximum" maximum0


{-| wraps drop
-}
drop : Expr
drop =
    let
        drop0 : FV
        drop0 =
            \c nExpr ->
                case getValue c nExpr of
                    VInt n ->
                        n
                            |> drop1
                            |> ComputedFunc

                    _ ->
                        error "first arg to drop should be an int"

        drop1 : Int -> FV
        drop1 =
            \n ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            lst
                                |> List.drop n
                                |> VList
                                |> ComputedValue

                        _ ->
                            error "drop wants a list"
    in
    NamedFunc "List.drop" drop0


{-| wraps take
-}
take : Expr
take =
    let
        take0 : FV
        take0 =
            \c nExpr ->
                case getValue c nExpr of
                    VInt n ->
                        n
                            |> take1
                            |> ComputedFunc

                    _ ->
                        error "first arg to take should be an int"

        take1 : Int -> FV
        take1 =
            \n ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            lst
                                |> List.take n
                                |> VList
                                |> ComputedValue

                        _ ->
                            error "take wants a list"
    in
    NamedFunc "List.take" take0


{-| wraps tail
-}
tail : Expr
tail =
    let
        tail0 : FV
        tail0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        lst
                            |> List.tail
                            |> Maybe.map VList
                            |> Maybe.map ComputedValue
                            |> VMaybe
                            |> ComputedValue

                    _ ->
                        error "tail wants a list"
    in
    NamedFunc "List.tail" tail0


{-| wraps head
-}
head : Expr
head =
    let
        head0 : FV
        head0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        lst
                            |> List.head
                            |> VMaybe
                            |> ComputedValue

                    _ ->
                        error "head wants a list"
    in
    NamedFunc "List.head" head0


{-| wraps isEmpty
-}
isEmpty : Expr
isEmpty =
    let
        isEmpty0 : FV
        isEmpty0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        lst
                            |> List.isEmpty
                            |> VBool
                            |> ComputedValue

                    _ ->
                        error "isEmpty wants a list"
    in
    NamedFunc "List.isEmpty" isEmpty0


{-| wraps length
-}
length : Expr
length =
    let
        length0 : FV
        length0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        lst
                            |> List.length
                            |> VInt
                            |> ComputedValue

                    _ ->
                        error "length wants a list"
    in
    NamedFunc "List.length" length0


{-| wraps reverse
-}
reverse : Expr
reverse =
    let
        reverse0 : FV
        reverse0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        lst
                            |> List.reverse
                            |> VList
                            |> ComputedValue

                    _ ->
                        error "reverse wants a list"
    in
    NamedFunc "List.reverse" reverse0


{-| wraps product
-}
product : Expr
product =
    A2 foldl MeNumber.mult (MeInt.init 1)
        |> Var "List.product"


{-| wraps sum
-}
sum : Expr
sum =
    A2 foldl MeNumber.plus (MeInt.init 0)
        |> Var "List.sum"


{-| wraps sort
-}
sort : Expr
sort =
    let
        sort0 : FV
        sort0 =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        transformSort compute c lst

                    _ ->
                        error "sort wants a list"
    in
    NamedFunc "List.sort" sort0


{-| wraps List.sortBy
-}
sortBy : Expr
sortBy =
    let
        sortBy0 : FV
        sortBy0 =
            \c ordExpr ->
                let
                    ord =
                        getFuncV c ordExpr
                in
                sortBy1 ord
                    |> ComputedFunc

        sortBy1 : FV -> FV
        sortBy1 =
            \ord ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            transformSort ord c lst

                        VError s ->
                            error ("bad list in sortBy: " ++ s)

                        _ ->
                            error "need list in sortBy"
    in
    NamedFunc "List.sortBy" sortBy0


{-| wraps List.foldl
-}
foldl : Expr
foldl =
    let
        foldl0 : FV
        foldl0 c accumExpr =
            foldl1 (getFuncVV c accumExpr)
                |> ComputedFunc

        foldl1 : FVV -> FV
        foldl1 accum =
            \c startVal ->
                foldl2 accum (compute c startVal)
                    |> ComputedFunc

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
    in
    NamedFunc "List.foldl" foldl0


{-| wraps List.foldr
-}
foldr : Expr
foldr =
    let
        foldr0 : FV
        foldr0 c accumExpr =
            foldr1 (getFuncVV c accumExpr)
                |> ComputedFunc

        foldr1 : FVV -> FV
        foldr1 accum =
            \c startVal ->
                foldr2 accum (compute c startVal)
                    |> ComputedFunc

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
    in
    NamedFunc "List.foldr" foldr0


{-| wraps List.filterMap
-}
filterMap : Expr
filterMap =
    let
        filterMap0 : FV
        filterMap0 =
            \c predExpr ->
                predExpr
                    |> getFuncV c
                    |> filterMap1
                    |> ComputedFunc

        filterMap1 : FV -> FV
        filterMap1 =
            \pred ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            lst
                                |> List.filter (makePredicate c pred)
                                |> VList
                                |> ComputedValue

                        VError s ->
                            error ("bad list in filterMap: " ++ s)

                        _ ->
                            error "need list in filterMap"
    in
    NamedFunc "List.filterMap" filterMap0


{-| wraps List.filter
-}
filter : Expr
filter =
    let
        filter0 : FV
        filter0 =
            \c predExpr ->
                predExpr
                    |> getFuncV c
                    |> filter1
                    |> ComputedFunc

        filter1 : FV -> FV
        filter1 =
            \pred ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            lst
                                |> List.filter (makePredicate c pred)
                                |> VList
                                |> ComputedValue

                        VError s ->
                            error ("bad list in filter: " ++ s)

                        _ ->
                            error "need list in filter"
    in
    NamedFunc "List.filter" filter0


{-| wraps List.any
-}
any : Expr
any =
    let
        any0 : FV
        any0 =
            \c predExpr ->
                predExpr
                    |> getFuncV c
                    |> any1
                    |> ComputedFunc

        any1 : FV -> FV
        any1 =
            \pred ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            lst
                                |> List.any (makePredicate c pred)
                                |> VBool
                                |> ComputedValue

                        VError s ->
                            error ("bad list in any: " ++ s)

                        _ ->
                            error "need list in any"
    in
    NamedFunc "List.any" any0


{-| wraps List.all
-}
all : Expr
all =
    let
        all0 : FV
        all0 =
            \c predExpr ->
                predExpr
                    |> getFuncV c
                    |> all1
                    |> ComputedFunc

        all1 : FV -> FV
        all1 =
            \pred ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            lst
                                |> List.all (makePredicate c pred)
                                |> VBool
                                |> ComputedValue

                        VError s ->
                            error ("bad list in all: " ++ s)

                        _ ->
                            error "need list in all"
    in
    NamedFunc "List.all" all0


{-| wraps List.map2
-}
map2 : Expr
map2 =
    let
        map2_0 : FV
        map2_0 =
            \c mapperExpr ->
                mapperExpr
                    |> getFuncVV c
                    |> map2_1
                    |> ComputedFunc

        map2_1 : FVV -> FV
        map2_1 =
            \mapper ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            map2_2 mapper lst
                                |> ComputedFunc

                        VError s ->
                            error ("bad list in map2: " ++ s)

                        _ ->
                            error "need list in map2"

        map2_2 : FVV -> List Expr -> FV
        map2_2 =
            \mapper lst1 ->
                \c lst2Expr ->
                    case getValue c lst2Expr of
                        VList lst2 ->
                            List.map2 (mapper c) lst1 lst2
                                |> VList
                                |> ComputedValue

                        VError s ->
                            error ("bad list in map2: " ++ s)

                        _ ->
                            error "need list in map2"
    in
    NamedFunc "List.map2" map2_0


{-| wraps List.map3
-}
map3 : Expr
map3 =
    let
        map3_0 : FV
        map3_0 =
            \c mapperExpr ->
                mapperExpr
                    |> getFuncVVV c
                    |> map3_1
                    |> ComputedFunc

        map3_1 : FVVV -> FV
        map3_1 =
            \mapper ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            map3_2 mapper lst
                                |> ComputedFunc

                        VError s ->
                            error ("bad list in map3: " ++ s)

                        _ ->
                            error "need list in map3"

        map3_2 : FVVV -> List Expr -> FV
        map3_2 =
            \mapper lst1 ->
                \c lst2Expr ->
                    case getValue c lst2Expr of
                        VList lst2 ->
                            map3_3 mapper lst1 lst2
                                |> ComputedFunc

                        VError s ->
                            error ("bad list in map3: " ++ s)

                        _ ->
                            error "need list in map3"

        map3_3 : FVVV -> List Expr -> List Expr -> FV
        map3_3 =
            \mapper lst1 lst2 ->
                \c lst3Expr ->
                    case getValue c lst3Expr of
                        VList lst3 ->
                            List.map3 (mapper c) lst1 lst2 lst3
                                |> VList
                                |> ComputedValue

                        VError s ->
                            error ("bad list in map3: " ++ s)

                        _ ->
                            error "need list in map3"
    in
    NamedFunc "List.map3" map3_0


{-| wraps List.map
-}
map : Expr
map =
    let
        map_0 : FV
        map_0 =
            \c mapperExpr ->
                mapperExpr
                    |> getFuncV c
                    |> map_1
                    |> ComputedFunc

        map_1 : FV -> FV
        map_1 =
            \mapper ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            happyPath c lst mapper

                        VError s ->
                            error ("bad list in map: " ++ s)

                        _ ->
                            error "need list in map"

        happyPath : Context -> List Expr -> FV -> Expr
        happyPath c lst mapper =
            lst
                |> List.map (mapper c)
                |> VList
                |> ComputedValue
    in
    NamedFunc "List.map" map_0


{-| wraps List.member
-}
member : Expr
member =
    let
        member0 : FV
        member0 =
            \c needle ->
                needle
                    |> compute c
                    |> member1
                    |> ComputedFunc

        member1 : Expr -> FV
        member1 =
            \needle ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            let
                                eq item =
                                    compare needle item == EQ
                            in
                            List.any eq lst
                                |> VBool
                                |> ComputedValue

                        _ ->
                            error "member needs a list for second arg"
    in
    NamedFunc "List.member" member0


{-| wraps List.repeat
-}
repeat : Expr
repeat =
    let
        repeat0 : Context -> Expr -> Expr
        repeat0 =
            \c nExpr ->
                case getValue c nExpr of
                    VInt n ->
                        n
                            |> repeat1
                            |> ComputedFunc

                    _ ->
                        error "first arg to repeat must be an integer"

        repeat1 : Int -> (Context -> Expr -> Expr)
        repeat1 =
            \n ->
                \c vExpr ->
                    vExpr
                        |> compute c
                        |> List.repeat n
                        |> VList
                        |> ComputedValue
    in
    NamedFunc "List.repeat" repeat0


{-| wraps List.range
-}
range : Expr
range =
    let
        range0 : Context -> Expr -> Expr
        range0 =
            \c loExpr ->
                case getValue c loExpr of
                    VInt lo ->
                        lo
                            |> range1
                            |> ComputedFunc

                    _ ->
                        error "low value to range must be integer"

        range1 : Int -> (Context -> Expr -> Expr)
        range1 =
            \lo ->
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
    in
    NamedFunc "List.range" range0


{-| wraps List.singleton
-}
singleton : Expr
singleton =
    let
        f : FV
        f c expr =
            expr
                |> compute c
                |> List.singleton
                |> VList
                |> ComputedValue
    in
    NamedFunc "List.singleton" f


{-| wraps cons (::)
-}
cons : Expr
cons =
    let
        cons0 : FV
        cons0 =
            \c hExpr ->
                hExpr
                    |> compute c
                    |> cons1
                    |> ComputedFunc

        cons1 : Expr -> FV
        cons1 =
            \h ->
                \c restExpr ->
                    case getValue c restExpr of
                        VList rest ->
                            h
                                :: rest
                                |> VList
                                |> ComputedValue

                        _ ->
                            error "need list to cons to"
    in
    OpFunc "cons" cons0 "::"


{-| make an empty list
-}
empty : Expr
empty =
    []
        |> VList
        |> ComputedValue


{-| wraps concat
-}
concat : Expr
concat =
    A2 foldr append empty
        |> Var "List.concat"


{-| wraps concatMap
-}
concatMap : Expr
concatMap =
    let
        unBoxList lstExpr =
            case lstExpr of
                ComputedValue v ->
                    case v of
                        VList lst ->
                            lst

                        _ ->
                            []

                _ ->
                    []

        concatMap0 : FV
        concatMap0 =
            \c fExpr ->
                fExpr
                    |> getFuncV c
                    |> concatMap1
                    |> ComputedFunc

        concatMap1 : FV -> FV
        concatMap1 =
            \f ->
                \c lstExpr ->
                    case getValue c lstExpr of
                        VList lst ->
                            lst
                                |> List.map (f c)
                                |> List.map unBoxList
                                |> List.concat
                                |> VList
                                |> ComputedValue

                        _ ->
                            error "was expecting a list"
    in
    NamedFunc "List.concatMap" concatMap0


{-| wraps append (++)
-}
append : Expr
append =
    let
        append0 : FV
        append0 =
            \c lstExpr1 ->
                case getValue c lstExpr1 of
                    VList lst ->
                        lst
                            |> append1
                            |> ComputedFunc

                    _ ->
                        error "first arg to append should be a list"

        append1 : List Expr -> FV
        append1 =
            \lst1 ->
                \c lstExpr2 ->
                    let
                        v2 =
                            getValue c lstExpr2
                    in
                    case v2 of
                        VList lst2 ->
                            (lst1 ++ lst2)
                                |> VList
                                |> ComputedValue

                        _ ->
                            error "second arg to append should be a list"
    in
    OpFunc "List.append" append0 "++"


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

                hExpr :: rExpr ->
                    case ( convert hExpr, f rExpr ) of
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


makePredicate : Context -> FV -> (Expr -> Bool)
makePredicate =
    \c pred ->
        \vExpr ->
            case getValue c (pred c vExpr) of
                VBool b ->
                    b

                VMaybe m ->
                    case m of
                        Just _ ->
                            True

                        _ ->
                            False

                _ ->
                    False


min : Expr -> Expr -> Expr
min vExpr1 vExpr2 =
    case ( getFinalValue vExpr1, getFinalValue vExpr2 ) of
        ( VInt n1, VInt n2 ) ->
            Basics.min n1 n2
                |> VInt
                |> ComputedValue

        ( VFloat n1, VFloat n2 ) ->
            Basics.min n1 n2
                |> VFloat
                |> ComputedValue

        _ ->
            VError "type not supported by min (yet)"
                |> ComputedValue


max : Expr -> Expr -> Expr
max vExpr1 vExpr2 =
    case ( getFinalValue vExpr1, getFinalValue vExpr2 ) of
        ( VInt n1, VInt n2 ) ->
            Basics.max n1 n2
                |> VInt
                |> ComputedValue

        ( VFloat n1, VFloat n2 ) ->
            Basics.max n1 n2
                |> VFloat
                |> ComputedValue

        _ ->
            VError "type not supported by max (yet)"
                |> ComputedValue


compare : Expr -> Expr -> Order
compare vExpr1 vExpr2 =
    case ( getFinalValue vExpr1, getFinalValue vExpr2 ) of
        ( VInt n1, VInt n2 ) ->
            Basics.compare n1 n2

        ( VFloat n1, VFloat n2 ) ->
            Basics.compare n1 n2

        _ ->
            Basics.EQ
