module MeList exposing
    ( cons
    , indexedMap
    , initInts
    , map
    , plus
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


indexedMap : Expr -> Expr
indexedMap mapperExpr =
    let
        happyPath : List Expr -> (Expr -> Expr -> Expr) -> Expr
        happyPath lst mapper =
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
                |> ComputedValue

        f c v =
            case ( getValue c v, getFuncVV c mapperExpr ) of
                ( VList lst, Ok mapper ) ->
                    happyPath lst (mapper c)

                ( _, Err s ) ->
                    error ("bad mapper" ++ s)

                _ ->
                    error "indexedMap wants a list"
    in
    ComposeF "List.indexedMap" mapperExpr f


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

        f : FV
        f c expr =
            case getValue c expr of
                VList lst ->
                    transformSort ord unbox c lst

                _ ->
                    error "sort wants a list"
    in
    FunctionV "List.sort" f


sortByInt : Expr -> Expr
sortByInt mapper =
    sortBy MeInt.toInt mapper


sortBy : (Expr -> Result String comparable1) -> Expr -> Expr
sortBy unbox ordExpr =
    let
        f : FV
        f c expr =
            case ( getValue c expr, getFuncV c ordExpr ) of
                ( VList lst, Ok ord ) ->
                    transformSort ord unbox c lst

                ( _, Err s ) ->
                    error ("bad mapper" ++ s)

                _ ->
                    error "sortBy wants a list"
    in
    ComposeF "List.sortBy" ordExpr f


map : Expr -> Expr
map mapperExpr =
    let
        happyPath : List Expr -> (Expr -> Expr) -> Expr
        happyPath lst mapper =
            lst
                |> List.map mapper
                |> VList
                |> ComputedValue

        f c expr =
            case ( getValue c expr, getFuncV c mapperExpr ) of
                ( VList lst, Ok mapper ) ->
                    happyPath lst (mapper c)

                ( _, Err s ) ->
                    error ("bad mapper: " ++ s)

                _ ->
                    error "map wants a list"
    in
    ComposeF "List.map" mapperExpr f


singleton : Expr
singleton =
    let
        f : FV
        f c expr =
            [ ComputedValue (getValue c expr) ]
                |> VList
                |> ComputedValue
    in
    FunctionV "List.singleton" f


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
