module MeList exposing
    ( indexedMap
    , initInts
    , map
    , prepend
    , sort
    , sortBy
    , sortByInt
    , sortInt
    , toList
    )

import MeInt
import MeRunTime exposing (..)
import MeType exposing (..)


toList : (V -> Result String a) -> V -> Result String (List a)
toList getItem listValue =
    let
        get : Expr -> Result String a
        get elem =
            case getFinalValue elem of
                Ok v ->
                    getItem v

                Err s ->
                    Err s

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
        happyPath : List Expr -> (Expr -> Expr -> V) -> V
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
                |> List.map ComputedValue
                |> VList

        f c v =
            case ( computeV c v, getFuncVV c mapperExpr ) of
                ( VList lst, Ok mapper ) ->
                    happyPath lst (mapper c)

                ( _, Err s ) ->
                    VError ("bad mapper" ++ s)

                _ ->
                    VError "indexedMap wants a list"
    in
    ComposeF "List.indexedMap" mapperExpr f


sortInt : Expr
sortInt =
    sort MeInt.toInt


transformSort : FV -> (V -> Result String comparable) -> Context -> List Expr -> V
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

        Err s ->
            VError s


sort : (V -> Result String comparable) -> Expr
sort unbox =
    let
        ord : FV
        ord c expr =
            computeV c expr

        f : FV
        f c expr =
            case computeV c expr of
                VList lst ->
                    transformSort ord unbox c lst

                _ ->
                    VError "sort wants a list"
    in
    FunctionV "List.sort" f


sortByInt : Expr -> Expr
sortByInt mapper =
    sortBy MeInt.toInt mapper


sortBy : (V -> Result String comparable1) -> Expr -> Expr
sortBy unbox ordExpr =
    let
        f : FV
        f c expr =
            case ( computeV c expr, getFuncV c ordExpr ) of
                ( VList lst, Ok ord ) ->
                    transformSort ord unbox c lst

                ( _, Err s ) ->
                    VError ("bad mapper" ++ s)

                _ ->
                    VError "sortBy wants a list"
    in
    ComposeF "List.sortBy" ordExpr f


map : Expr -> Expr
map mapperExpr =
    let
        happyPath : List Expr -> (Expr -> V) -> V
        happyPath lst mapper =
            lst
                |> List.map mapper
                |> List.map ComputedValue
                |> VList

        f c expr =
            case ( computeV c expr, getFuncV c mapperExpr ) of
                ( VList lst, Ok mapper ) ->
                    happyPath lst (mapper c)

                ( _, Err s ) ->
                    VError ("bad mapper: " ++ s)

                _ ->
                    VError "map wants a list"
    in
    ComposeF "List.map" mapperExpr f


prepend : Expr
prepend =
    let
        f : FVV
        f c expr1 expr2 =
            case ( computeV c expr1, computeV c expr2 ) of
                ( VError s, _ ) ->
                    VError ("bad arg to :: - " ++ s)

                ( h, VList lst ) ->
                    VList (expr1 :: lst)

                ( _, _ ) ->
                    VError "need list to prepend to"
    in
    BinOp "::" f


initInts : List Int -> Expr
initInts nums =
    nums
        |> List.map MeInt.init
        |> VList
        |> SimpleValue
