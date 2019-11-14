module MeRunTime exposing
    ( computeExpr, compute, getFinalValue
    , getFuncV, getFuncVV, getValue, error
    )

{-| The MetaElm RunTime (MeRunTime) can evaluate
AST expressions inside Elm!


# main API

@docs computeExpr, compute, getFinalValue


# helpers

Helpers are mostly used for wrapping library functions
like List.map.

@docs getFuncV, getFuncVV, getValue, error

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
    case expr of
        Function params _ ->
            if List.length params == List.length args then
                List.map2 Tuple.pair params args
                    |> Dict.fromList
                    |> Ok

            else
                Err "wrong number of arguments"

        _ ->
            Err "you must call a function"


{-| like computeExpr, but you can pass in a context
-}
compute : FV
compute context expr =
    let
        apply : Expr -> Expr -> Expr
        apply e1 e0 =
            let
                fv =
                    getFuncV context e1
            in
            fv context e0
    in
    case expr of
        LetIn c resultExpr ->
            compute (union (fromList c) context) resultExpr

        Var _ v ->
            compute context v

        Function _ v ->
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
                BinOp _ fvv ->
                    fvv
                        context
                        (opLeft |> compute context)
                        (opRight |> compute context)

                _ ->
                    error "infix needs a binary operator: "

        A2 e2 e1 e0 ->
            apply (apply e2 e1) e0

        A1 e1 e0 ->
            apply e1 e0

        _ ->
            error "cannot evaluate this type as a value yet"


{-| get an expression that wraps a function taking two arguments
-}
getFuncVV : Context -> Expr -> FVV
getFuncVV c expr =
    case expr of
        BinOp _ fvv ->
            fvv

        _ ->
            let
                fv1 =
                    getFuncV c expr
            in
            \_ e1 e2 ->
                case fv1 c e1 of
                    ComputedFunc fv2 ->
                        fv2 c e2

                    _ ->
                        error "could not compute function with two args"


{-| get an expression that wraps a function taking one argument
-}
getFuncV : Context -> Expr -> FV
getFuncV context expr =
    let
        err s =
            \_ _ ->
                VError s
                    |> ComputedValue
    in
    case expr of
        NamedFunc _ f ->
            f

        ComputedFunc f ->
            f

        LambdaLeft _ binOp opRight ->
            case binOp of
                BinOp _ fvv ->
                    let
                        fv c opLeft =
                            fvv c opLeft opRight
                    in
                    fv

                _ ->
                    err "lambda left needs a binary operator"

        LambdaRight opLeft binOp _ ->
            case binOp of
                BinOp _ fvv ->
                    let
                        fv c opRight =
                            fvv c opLeft opRight
                    in
                    fv

                _ ->
                    err "lambda right needs a binary operator"

        ComputedValue _ ->
            err "not a function"

        SimpleValue _ ->
            err "not a function"

        _ ->
            compute context expr
                |> getFuncV context


evalPipeLine : Context -> Expr -> List Expr -> Expr
evalPipeLine context v lst =
    case lst of
        [] ->
            compute context v

        head :: rest ->
            let
                fv =
                    getFuncV context head

                newV =
                    fv context v
            in
            evalPipeLine context newV rest


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
            let
                _ =
                    Debug.log "yo" expr
            in
            VError "final values were never computed with computeExpr"
