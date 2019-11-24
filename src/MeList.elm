module MeList exposing
    ( initInts, initFloats, empty
    , toList, toListInts
    , cons, append
    , all, any, concat, concatMap, drop, filter, filterMap, foldl, foldr, head, indexedMap, intersperse, isEmpty, length, map, map2, map3, map4, map5, maximum, member, minimum, partition, product, range, repeat, reverse, singleton, sort, sortBy, sortWith, sum, tail, take, unzip
    )

{-| wrap List


# conversion (in)

@docs initInts, initFloats, empty


# conversion (out)

@docs toList, toListInts


# operators

@docs cons, append


# wrappers

@docs all, any, concat, concatMap, drop, filter, filterMap, foldl, foldr, head, indexedMap, intersperse, isEmpty, length, map, map2, map3, map4, map5, maximum, member, minimum, partition, product, range, repeat, reverse, singleton, sort, sortBy, sortWith, sum, tail, take, unzip

-}

import MeApply
import MeBasics
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
                MeApply.list <|
                    \c lst ->
                        let
                            wrapped_mapper idx item =
                                let
                                    idxExpr =
                                        idx
                                            |> VInt
                                            |> ComputedValue
                                in
                                mapper c idxExpr item
                        in
                        lst
                            |> List.indexedMap wrapped_mapper
                            |> VList
    in
    NamedFunc "List.indexedMap" indexedMap0


transformSort : FV -> Context -> List Expr -> V
transformSort ord c lst =
    let
        comp item1 item2 =
            compare c (Tuple.first item1) (Tuple.first item2)
    in
    lst
        |> List.map (\item -> ( ord c item, item ))
        |> List.sortWith comp
        |> List.map Tuple.second
        |> VList


{-| wraps List.minimum
-}
minimum : Expr
minimum =
    let
        minimum0 : FV
        minimum0 =
            MeApply.list <|
                \_ lst ->
                    case lst of
                        x :: xs ->
                            Just (List.foldl min x xs)
                                |> VMaybe

                        _ ->
                            Nothing
                                |> VMaybe
    in
    NamedFunc "List.minimum" minimum0


{-| wraps List.maximum
-}
maximum : Expr
maximum =
    let
        maximum0 : FV
        maximum0 =
            MeApply.list <|
                \_ lst ->
                    case lst of
                        x :: xs ->
                            Just (List.foldl max x xs)
                                |> VMaybe

                        _ ->
                            Nothing
                                |> VMaybe
    in
    NamedFunc "List.maximum" maximum0


{-| wraps List.intersperse
-}
intersperse : Expr
intersperse =
    let
        intersperse0 : FV
        intersperse0 =
            MeApply.exprFV intersperse1

        intersperse1 : Expr -> FV
        intersperse1 =
            \v ->
                MeApply.list <|
                    \_ lst ->
                        List.intersperse v lst
                            |> VList
    in
    NamedFunc "List.intersperse" intersperse0


{-| wraps List.drop
-}
drop : Expr
drop =
    let
        drop0 : FV
        drop0 =
            MeApply.intFV drop1

        drop1 : Int -> FV
        drop1 =
            \n ->
                MeApply.list <|
                    \_ lst ->
                        lst
                            |> List.drop n
                            |> VList
    in
    NamedFunc "List.drop" drop0


{-| wraps List.take
-}
take : Expr
take =
    let
        take0 : FV
        take0 =
            MeApply.intFV take1

        take1 : Int -> FV
        take1 =
            \n ->
                MeApply.list <|
                    \_ lst ->
                        lst
                            |> List.take n
                            |> VList
    in
    NamedFunc "List.take" take0


{-| wraps List.unzip
-}
unzip : Expr
unzip =
    let
        unzip0 : FV
        unzip0 =
            MeApply.list <|
                \c lst ->
                    let
                        first =
                            Tuple.first
                                |> MeApply.tuple

                        second =
                            Tuple.second
                                |> MeApply.tuple

                        lst1 =
                            lst
                                |> List.map (first c)
                                |> VList
                                |> ComputedValue

                        lst2 =
                            lst
                                |> List.map (second c)
                                |> VList
                                |> ComputedValue
                    in
                    ( lst1, lst2 )
                        |> VTuple
    in
    NamedFunc "List.unzip" unzip0


{-| wraps List.tail
-}
tail : Expr
tail =
    let
        tail0 : FV
        tail0 =
            MeApply.list <|
                \_ lst ->
                    lst
                        |> List.tail
                        |> Maybe.map VList
                        |> Maybe.map ComputedValue
                        |> VMaybe
    in
    NamedFunc "List.tail" tail0


{-| wraps List.head
-}
head : Expr
head =
    let
        head0 : FV
        head0 =
            MeApply.list <|
                \_ lst ->
                    lst
                        |> List.head
                        |> VMaybe
    in
    NamedFunc "List.head" head0


{-| wraps List.isEmpty
-}
isEmpty : Expr
isEmpty =
    let
        isEmpty0 : FV
        isEmpty0 =
            MeApply.list <|
                \_ lst ->
                    lst
                        |> List.isEmpty
                        |> VBool
    in
    NamedFunc "List.isEmpty" isEmpty0


{-| wraps List.length
-}
length : Expr
length =
    let
        length0 : FV
        length0 =
            MeApply.list <|
                \_ lst ->
                    lst
                        |> List.length
                        |> VInt
    in
    NamedFunc "List.length" length0


{-| wraps List.reverse
-}
reverse : Expr
reverse =
    let
        reverse0 : FV
        reverse0 =
            MeApply.list <|
                \_ lst ->
                    lst
                        |> List.reverse
                        |> VList
    in
    NamedFunc "List.reverse" reverse0


{-| wraps List.product
-}
product : Expr
product =
    A2 foldl MeNumber.mult (MeInt.init 1)
        |> Var "List.product"


{-| wraps List.sum
-}
sum : Expr
sum =
    A2 foldl MeNumber.plus (MeInt.init 0)
        |> Var "List.sum"


{-| wraps List.sort
-}
sort : Expr
sort =
    let
        sort0 : FV
        sort0 =
            MeApply.list <|
                \c lst ->
                    transformSort compute c lst
    in
    NamedFunc "List.sort" sort0


{-| wraps List.sortWith
-}
sortWith : Expr
sortWith =
    let
        sortWith0 : FV
        sortWith0 =
            \_ withExpr ->
                withExpr
                    |> sortWith1
                    |> ComputedFunc

        sortWith1 : Expr -> FV
        sortWith1 =
            \withExpr ->
                MeApply.list <|
                    \c lst ->
                        let
                            with a b =
                                getFuncVV c withExpr c a b
                                    |> toOrderUnsafe
                        in
                        lst
                            |> List.sortWith with
                            |> VList
    in
    NamedFunc "List.sortWith" sortWith0


{-| wraps List.sortBy
-}
sortBy : Expr
sortBy =
    let
        sortBy0 : FV
        sortBy0 =
            MeApply.fvFV sortBy1

        sortBy1 : FV -> FV
        sortBy1 =
            \ord ->
                MeApply.list <|
                    \c lst ->
                        transformSort ord c lst
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
            MeApply.listExpr <|
                \c lst ->
                    List.foldl (accum c) startVal lst
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
            MeApply.listExpr <|
                \c lst ->
                    List.foldr (accum c) startVal lst
    in
    NamedFunc "List.foldr" foldr0


{-| wraps List.filterMap
-}
filterMap : Expr
filterMap =
    let
        filterMap0 : FV
        filterMap0 =
            MeApply.fvFV filterMap1

        filterMap1 : FV -> FV
        filterMap1 =
            \pred ->
                MeApply.list <|
                    \c lst ->
                        lst
                            |> List.filterMap (maybePredicate c pred)
                            |> VList
    in
    NamedFunc "List.filterMap" filterMap0


{-| wraps List.filter
-}
filter : Expr
filter =
    let
        filter0 : FV
        filter0 =
            MeApply.fvFV filter1

        filter1 : FV -> FV
        filter1 =
            \pred ->
                MeApply.list <|
                    \c lst ->
                        lst
                            |> List.filter (makePredicate c pred)
                            |> VList
    in
    NamedFunc "List.filter" filter0


{-| wraps List.partition
-}
partition : Expr
partition =
    let
        partition0 : FV
        partition0 =
            MeApply.fvFV partition1

        partition1 : FV -> FV
        partition1 =
            \pred ->
                MeApply.list <|
                    \c lst ->
                        lst
                            |> List.partition (makePredicate c pred)
                            |> Tuple.mapBoth VList VList
                            |> Tuple.mapBoth ComputedValue ComputedValue
                            |> VTuple
    in
    NamedFunc "List.partition" partition0


{-| wraps List.any
-}
any : Expr
any =
    let
        any0 : FV
        any0 =
            MeApply.fvFV any1

        any1 : FV -> FV
        any1 =
            \pred ->
                MeApply.list <|
                    \c lst ->
                        lst
                            |> List.any (makePredicate c pred)
                            |> VBool
    in
    NamedFunc "List.any" any0


{-| wraps List.all
-}
all : Expr
all =
    let
        all0 : FV
        all0 =
            MeApply.fvFV all1

        all1 : FV -> FV
        all1 =
            \pred ->
                MeApply.list <|
                    \c lst ->
                        lst
                            |> List.all (makePredicate c pred)
                            |> VBool
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
                MeApply.listExpr <|
                    \_ lst1 ->
                        map2_2 mapper lst1
                            |> ComputedFunc

        map2_2 : FVV -> List Expr -> FV
        map2_2 =
            \mapper lst1 ->
                MeApply.list <|
                    \c lst2 ->
                        List.map2 (mapper c) lst1 lst2
                            |> VList
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
                MeApply.listExpr <|
                    \_ lst1 ->
                        map3_2 mapper lst1
                            |> ComputedFunc

        map3_2 : FVVV -> List Expr -> FV
        map3_2 =
            \mapper lst1 ->
                MeApply.listExpr <|
                    \_ lst2 ->
                        map3_3 mapper lst1 lst2
                            |> ComputedFunc

        map3_3 : FVVV -> List Expr -> List Expr -> FV
        map3_3 =
            \mapper lst1 lst2 ->
                MeApply.list <|
                    \c lst3 ->
                        List.map3 (mapper c) lst1 lst2 lst3
                            |> VList
    in
    NamedFunc "List.map3" map3_0


{-| wraps List.map4
-}
map4 : Expr
map4 =
    let
        map4_0 : FV
        map4_0 =
            \c mapperExpr ->
                mapperExpr
                    |> getFuncVVVV c
                    |> map4_1
                    |> ComputedFunc

        map4_1 : FVVVV -> FV
        map4_1 =
            \mapper ->
                MeApply.listExpr <|
                    \_ lst1 ->
                        map4_2 mapper lst1
                            |> ComputedFunc

        map4_2 : FVVVV -> List Expr -> FV
        map4_2 =
            \mapper lst1 ->
                MeApply.listExpr <|
                    \_ lst2 ->
                        map4_3 mapper lst1 lst2
                            |> ComputedFunc

        map4_3 : FVVVV -> List Expr -> List Expr -> FV
        map4_3 =
            \mapper lst1 lst2 ->
                MeApply.listExpr <|
                    \_ lst3 ->
                        map4_4 mapper lst1 lst2 lst3
                            |> ComputedFunc

        map4_4 : FVVVV -> List Expr -> List Expr -> List Expr -> FV
        map4_4 =
            \mapper lst1 lst2 lst3 ->
                MeApply.list <|
                    \c lst4 ->
                        List.map4 (mapper c) lst1 lst2 lst3 lst4
                            |> VList
    in
    NamedFunc "List.map4" map4_0


{-| wraps List.map5
-}
map5 : Expr
map5 =
    let
        map5_0 : FV
        map5_0 =
            \c mapperExpr ->
                mapperExpr
                    |> getFuncVVVVV c
                    |> map5_1
                    |> ComputedFunc

        map5_1 : FVVVVV -> FV
        map5_1 =
            \mapper ->
                MeApply.listExpr <|
                    \_ lst1 ->
                        map5_2 mapper lst1
                            |> ComputedFunc

        map5_2 : FVVVVV -> List Expr -> FV
        map5_2 =
            \mapper lst1 ->
                MeApply.listExpr <|
                    \_ lst2 ->
                        map5_3 mapper lst1 lst2
                            |> ComputedFunc

        map5_3 : FVVVVV -> List Expr -> List Expr -> FV
        map5_3 =
            \mapper lst1 lst2 ->
                MeApply.listExpr <|
                    \_ lst3 ->
                        map5_4 mapper lst1 lst2 lst3
                            |> ComputedFunc

        map5_4 : FVVVVV -> List Expr -> List Expr -> List Expr -> FV
        map5_4 =
            \mapper lst1 lst2 lst3 ->
                MeApply.listExpr <|
                    \_ lst4 ->
                        map5_5 mapper lst1 lst2 lst3 lst4
                            |> ComputedFunc

        map5_5 : FVVVVV -> List Expr -> List Expr -> List Expr -> List Expr -> FV
        map5_5 =
            \mapper lst1 lst2 lst3 lst4 ->
                MeApply.list <|
                    \c lst5 ->
                        List.map5 (mapper c) lst1 lst2 lst3 lst4 lst5
                            |> VList
    in
    NamedFunc "List.map5" map5_0


{-| wraps List.map
-}
map : Expr
map =
    let
        map_0 : FV
        map_0 =
            MeApply.fvFV map_1

        map_1 : FV -> FV
        map_1 =
            \mapper ->
                MeApply.list <|
                    \c lst ->
                        lst
                            |> List.map (mapper c)
                            |> VList
    in
    NamedFunc "List.map" map_0


{-| wraps List.member
-}
member : Expr
member =
    let
        member0 : FV
        member0 =
            MeApply.exprFV member1

        member1 : Expr -> FV
        member1 =
            \needle ->
                MeApply.list <|
                    \c lst ->
                        let
                            eq item =
                                -- TODO, add Basics.eq
                                compare c needle item == EQ
                        in
                        List.any eq lst
                            |> VBool
    in
    NamedFunc "List.member" member0


{-| wraps List.repeat
-}
repeat : Expr
repeat =
    let
        repeat0 : Context -> Expr -> Expr
        repeat0 =
            MeApply.intFV repeat1

        repeat1 : Int -> FV
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
            MeApply.intFV range1

        range1 : Int -> FV
        range1 =
            \lo ->
                MeApply.int <|
                    \_ hi ->
                        List.range lo hi
                            |> List.map VInt
                            |> List.map ComputedValue
                            |> VList
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


{-| wraps List.cons (::)
-}
cons : Expr
cons =
    let
        cons0 : FV
        cons0 =
            MeApply.exprFV cons1

        cons1 : Expr -> FV
        cons1 =
            \h ->
                MeApply.list <|
                    \_ rest ->
                        h
                            :: rest
                            |> VList
    in
    OpFunc "List.cons" cons0 "::"


{-| make an empty list
-}
empty : Expr
empty =
    []
        |> VList
        |> ComputedValue


{-| wraps List.concat
-}
concat : Expr
concat =
    A2 foldr append empty
        |> Var "List.concat"


{-| wraps List.concatMap
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
            MeApply.fvFV concatMap1

        concatMap1 : FV -> FV
        concatMap1 =
            \f ->
                MeApply.list <|
                    \c lst ->
                        lst
                            |> List.map (f c)
                            |> List.map unBoxList
                            |> List.concat
                            |> VList
    in
    NamedFunc "List.concatMap" concatMap0


{-| wraps List.append (++)
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
                MeApply.list <|
                    \_ lst2 ->
                        (lst1 ++ lst2)
                            |> VList
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


maybePredicate : Context -> FV -> (Expr -> Maybe Expr)
maybePredicate =
    \c pred ->
        \vExpr ->
            case getValue c (pred c vExpr) of
                VMaybe m ->
                    m

                _ ->
                    Nothing


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


compare : Context -> Expr -> Expr -> Order
compare c expr1 expr2 =
    getFuncVV c MeBasics.compare c expr1 expr2
        |> toOrderUnsafe


toOrderUnsafe : Expr -> Order
toOrderUnsafe expr =
    {--This is a necessary evil when we talk to functions
        like native List.sortWith.  There's no way to reuse
        the code of List.sortWith without actually returning
        EQ, LT, or GT.  If our callers give us a bad
        ordBy/with function, we just have to return EQ.
    --}
    let
        err () =
            let
                _ =
                    Debug.log "bad expression for toOrderUnsafe" expr
            in
            EQ
    in
    case expr of
        ComputedValue v ->
            case v of
                VOrder ord ->
                    ord

                _ ->
                    err ()

        _ ->
            err ()
