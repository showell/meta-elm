module MeList exposing
    ( indexedMap
    , initInts
    , map
    , sortBy
    , sortByInt
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
        happy_path : List Expr -> (Expr -> Expr -> V) -> V
        happy_path lst mapper =
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
                    happy_path lst (mapper c)

                ( _, Err s ) ->
                    VError ("bad mapper" ++ s)

                _ ->
                    VError "indexedMap wants a list"
    in
    ComposeF "List.indexedMap" mapperExpr f


sortByInt : Expr -> Expr
sortByInt mapper =
    sortBy MeInt.toInt 0 mapper


sortBy : (V -> Result String comparable1) -> comparable1 -> Expr -> Expr
sortBy unbox default ordExpr =
    {--
        This is a bit of work in progress.  There are a few
        somewhat related problems:

            - We don't report errors if any values are
              the wrong type.  (If one value is wrong, all
              values should be wrong, and our `ord` will just
              return the same value each time, which means the
              list will randomly sort.

            - It's inefficient, as I do O(NlogN) unboxings.

            - I have to pass in the hacky `default` thingy to
              satisfy the compiler.

        This is easy enough to fix--I should just unbox the list
        first, which solves all the above problems.

            - When unboxing, it's easy to return VError if
              any bad element is encountered.  (Now I can't do
              it, because List.sortBy is opaque to me.)

            - I'll only do O(N) unbox/box operations. (2N total)

            - I won't need the `default`.

        The approach above will still use List.sortBy for the
        heavy lifting (which is basically Elm.Kernel.List.sortBy).
        It will require me to re-box everything in O(N) time.
    --}
    let
        happy_path : List Expr -> (Expr -> V) -> V
        happy_path lst ord =
            let
                rawOrd item =
                    case unbox (ord item) of
                        Ok data ->
                            data

                        _ ->
                            default
            in
            lst
                |> List.sortBy rawOrd
                |> VList

        f : FV
        f c expr =
            case ( computeV c expr, getFuncV c ordExpr ) of
                ( VList lst, Ok ord ) ->
                    happy_path lst (ord c)

                ( _, Err s ) ->
                    VError ("bad mapper" ++ s)

                _ ->
                    VError "sortBy wants a list"
    in
    ComposeF "List.sortBy" ordExpr f


map : Expr -> Expr
map mapperExpr =
    let
        happy_path : List Expr -> (Expr -> V) -> V
        happy_path lst mapper =
            lst
                |> List.map mapper
                |> List.map ComputedValue
                |> VList

        f c expr =
            case ( computeV c expr, getFuncV c mapperExpr ) of
                ( VList lst, Ok mapper ) ->
                    happy_path lst (mapper c)

                ( _, Err s ) ->
                    VError ("bad mapper: " ++ s)

                _ ->
                    VError "map wants a list"
    in
    ComposeF "List.map" mapperExpr f


initInts : List Int -> Expr
initInts nums =
    nums
        |> List.map MeInt.init
        |> VList
        |> SimpleValue
