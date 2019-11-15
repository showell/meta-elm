module MeRunTime exposing
    ( computeExpr, compute, getFinalValue
    , getFuncV, getFuncVV, getFuncVVV, getValue, error
    , getFuncVVVV
    )

{-| The MetaElm RunTime (MeRunTime) can evaluate
AST expressions inside Elm!


# main API

@docs computeExpr, compute, getFinalValue


# helpers

Helpers are mostly used for wrapping library functions
like List.map.

@docs getFuncV, getFuncVV, getFuncVVV, getValue, error

-}

import Dict
import MeType exposing (..)


{-| compute/evaluate an expression
-}
computeExpr : Expr -> Expr
computeExpr expr =
    let
        context =
            Dict.empty
    in
    compute context expr


{-| value representing error in computation
-}
error : String -> Expr
error s =
    ComputedValue (VError s)


{-| convert expression to value (or error if it's
a function
-}
getValue : Context -> Expr -> V
getValue context expr =
    case compute context expr of
        ComputedValue v ->
            v

        SimpleValue v ->
            v

        _ ->
            VError "trying to use uncomputed value"


union : Context -> Context -> Context
union new old =
    Dict.union new old


get : String -> Context -> Maybe Expr
get vname context =
    Dict.get vname context


fromList : List ( String, Expr ) -> Context
fromList tups =
    Dict.fromList tups


getArgDict : List Expr -> Expr -> Result String Context
getArgDict args expr =
    let
        getDict params =
            if List.length params == List.length args then
                List.map2 Tuple.pair params args
                    |> Dict.fromList
                    |> Ok

            else
                Err "wrong number of arguments"
    in
    case expr of
        F2 name1 name0 _ ->
            getDict [ name1, name0 ]

        F1 name0 _ ->
            getDict [ name0 ]

        _ ->
            Err "you must call a function"


{-| like computeExpr, but you can pass in a context
-}
compute : FV
compute context expr =
    case expr of
        LetIn c resultExpr ->
            compute (union (fromList c) context) resultExpr

        Var _ v ->
            compute context v

        F1 _ v ->
            compute context v

        VarName vname ->
            case get vname context of
                Just v ->
                    compute context v

                Nothing ->
                    error ("cannot find " ++ vname)

        ComputedValue v ->
            ComputedValue v

        SimpleValue v ->
            case v of
                VList lst ->
                    lst
                        |> List.map (compute context)
                        |> VList
                        |> ComputedValue

                VTuple ( a, b ) ->
                    ( a |> compute context
                    , b |> compute context
                    )
                        |> VTuple
                        |> ComputedValue

                _ ->
                    ComputedValue v

        PipeLine topExpr lst ->
            evalPipeLine context topExpr lst

        Call funcName args ->
            compute context (FuncCall context funcName args)

        FuncCall ns funcName args ->
            let
                newContext =
                    union ns context
            in
            case get funcName newContext of
                Just impl ->
                    let
                        computedArgs =
                            List.map (compute newContext) args
                    in
                    case getArgDict computedArgs impl of
                        Ok argDict ->
                            compute (union argDict newContext) impl

                        Err s ->
                            error ("bad args for " ++ funcName ++ ": " ++ s)

                Nothing ->
                    error ("cannot find name in module: " ++ funcName)

        IfElse cond ifExpr elseExpr ->
            case getValue context cond of
                VBool b ->
                    if b then
                        compute context ifExpr

                    else
                        compute context elseExpr

                VError s ->
                    error ("error with if conditional: " ++ s)

                _ ->
                    error "if needs a conditional"

        Infix opLeft binOp opRight ->
            case binOp of
                OpFunc _ fv _ ->
                    getFuncVV context binOp context opLeft opRight

                _ ->
                    error "infix needs a binary operator: "

        A5 e5 e4 e3 e2 e1 e0 ->
            getFuncVVVVV context e5 context e4 e3 e2 e1 e0

        A4 e4 e3 e2 e1 e0 ->
            getFuncVVVV context e4 context e3 e2 e1 e0

        A3 e3 e2 e1 e0 ->
            getFuncVVV context e3 context e2 e1 e0

        A2 e2 e1 e0 ->
            getFuncVV context e2 context e1 e0

        A1 e1 e0 ->
            getFuncV context e1 context e0

        _ ->
            let
                _ =
                    Debug.log "fred" expr
            in
            error "cannot evaluate this type as a value yet"


applyArgsToFunction : Context -> Expr -> List Expr -> Expr -> Expr
applyArgsToFunction c firstExpr exprs finalExpr =
    let
        fvInit =
            getFuncV c firstExpr

        apply arg fv =
            case fv c arg of
                ComputedFunc newF ->
                    newF

                _ ->
                    \_ _ ->
                        error "could not compute function"
    in
    let
        partial =
            List.foldl apply fvInit exprs
    in
    partial c finalExpr


callF : Context -> List String -> List Expr -> Expr -> Expr
callF c names exprs impl =
    let
        argDict =
            List.map2
                (\name expr -> ( name, compute c expr ))
                names
                exprs
                |> Dict.fromList
    in
    compute (union argDict c) impl


{-| kinda gets a five-argument function from an expression
-}
getFuncVVVVV : Context -> Expr -> FVVVVV
getFuncVVVVV c expr =
    case expr of
        F5 name4 name3 name2 name1 name0 impl ->
            \_ e4 e3 e2 e1 e0 ->
                callF c
                    [ name4, name3, name2, name1, name0 ]
                    [ e4, e3, e2, e1, e0 ]
                    impl

        _ ->
            \_ e4 e3 e2 e1 e0 ->
                applyArgsToFunction c expr [ e4, e3, e2, e1 ] e0


{-| kinda gets a four-argument function from an expression
-}
getFuncVVVV : Context -> Expr -> FVVVV
getFuncVVVV c expr =
    case expr of
        F4 name3 name2 name1 name0 impl ->
            \_ e3 e2 e1 e0 ->
                callF c
                    [ name3, name2, name1, name0 ]
                    [ e3, e2, e1, e0 ]
                    impl

        _ ->
            \_ e3 e2 e1 e0 ->
                applyArgsToFunction c expr [ e3, e2, e1 ] e0


{-| kinda gets a three-argument function from an expression
-}
getFuncVVV : Context -> Expr -> FVVV
getFuncVVV c expr =
    case expr of
        F3 name2 name1 name0 impl ->
            \_ e2 e1 e0 ->
                callF c
                    [ name2, name1, name0 ]
                    [ e2, e1, e0 ]
                    impl

        _ ->
            \_ e2 e1 e0 ->
                applyArgsToFunction c expr [ e2, e1 ] e0


{-| kinda gets a two-argument function from an expression
-}
getFuncVV : Context -> Expr -> FVV
getFuncVV c expr =
    case expr of
        F2 name1 name0 impl ->
            \_ e1 e0 ->
                callF c
                    [ name1, name0 ]
                    [ e1, e0 ]
                    impl

        _ ->
            \_ e1 e0 ->
                applyArgsToFunction c expr [ e1 ] e0


{-| kinda gets a one-argument function from an expression
-}
getFuncV : Context -> Expr -> FV
getFuncV c expr =
    let
        err s =
            \_ _ ->
                VError s
                    |> ComputedValue
    in
    case expr of
        F1 name impl ->
            \_ e ->
                callF c
                    [ name ]
                    [ e ]
                    impl

        OpFunc _ f _ ->
            f

        NamedFunc _ f ->
            f

        ComputedFunc f ->
            f

        ComputedValue _ ->
            err "not a function"

        SimpleValue _ ->
            err "not a function"

        _ ->
            compute c expr
                |> getFuncV c


evalPipeLine : Context -> Expr -> List Expr -> Expr
evalPipeLine context initVal fList =
    let
        accVal =
            compute context initVal

        accum fexpr val =
            getFuncV context fexpr context val
    in
    List.foldl accum accVal fList


{-| get the value of a computed expression (usually
after a call to `compute`)
-}
getFinalValue : Expr -> V
getFinalValue expr =
    case expr of
        ComputedValue v ->
            v

        SimpleValue v ->
            v

        _ ->
            VError "final values were never computed with computeExpr"
