module MeList exposing
    ( cons
    , indexedMap
    , initInts
    , map
    , plus
    , range
    , singleton
    , sort
    , sortBy
    , sortByInt
    , sortFloat
    , sortInt
    , toList
    )

import MeFloat
import MeInt
import MeRunTime exposing (..)
import MeType exposing (..)


toList : (V -> Result String a) -> V -> Result String (List a)
toList getItem listValue =
    let
        get : Expr -> Result String a
        get elem =
            case getFinalValue elem of
                VError s ->
                    Err s

                other ->
                    getItem other

        getItems : List Expr -> Result String (List a)
        getItems items =
            case items of
                [] ->
                    Ok []

                headV :: restV ->
                    case ( get headV, getItems restV ) of
                        ( Ok head, Ok rest ) ->
                            Ok (head :: rest)

                        ( Err s, _ ) ->
                            Err ("item is malformed: " ++ s)

                        ( _, Err s ) ->
                            Err s
    in
    case listValue of
        VList items ->
            getItems items

        _ ->
            Err "this is not a list value"


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


sortFloat : Expr
sortFloat =
    sort MeFloat.toFloat


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


sortByInt : Expr
sortByInt =
    sortBy MeInt.toInt


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
            case getFuncV c ordExpr of
                Ok ord ->
                    sortBy1 ord
                        |> ComputedFunc

                Err s ->
                    error ("bad ord for sortBy: " ++ s)
    in
    NamedFunc "List.sortBy" sortBy0


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
            case getFuncV c mapperExpr of
                Ok mapper ->
                    map1 mapper
                        |> ComputedFunc

                Err s ->
                    error ("bad mapper: " ++ s)
    in
    NamedFunc "List.map" map0


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


initInts : List Int -> Expr
initInts nums =
    nums
        |> List.map MeInt.init
        |> VList
        |> SimpleValue
