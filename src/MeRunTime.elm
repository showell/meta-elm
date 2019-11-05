module MeRunTime exposing
    ( computeV
    , computeVal
    , error
    , getFinalValue
    , getFuncV
    , getFuncVV
    , getValue
    )

import List.Extra
import MeType exposing (..)


computeVal : Expr -> Expr
computeVal expr =
    let
        context =
            []
    in
    computeV context expr


error : String -> Expr
error s =
    ComputedValue (VError s)


getValue : Context -> Expr -> V
getValue context expr =
    case computeV context expr of
        ComputedValue v ->
            v

        SimpleValue v ->
            v

        _ ->
            VError "trying to use uncomputed value"


computeV : FV
computeV context expr =
    case expr of
        LetIn c resultExpr ->
            computeV (c ++ context) resultExpr

        Var _ v ->
            computeV context v

        VarName vname ->
            let
                tup =
                    List.Extra.find (\( n, _ ) -> n == vname) context
            in
            case tup of
                Just ( _, v ) ->
                    computeV context v

                Nothing ->
                    error ("cannot find " ++ vname)

        ComputedValue v ->
            ComputedValue v

        SimpleValue v ->
            SimpleValue v

        PipeLine topExpr lst ->
            evalPipeLine context topExpr lst

        Call funcName args ->
            let
                funcImpl =
                    List.Extra.find (\( n, _ ) -> n == funcName) context
                        |> Maybe.map Tuple.second

                computedArgs =
                    args
                        |> List.map
                            (\( n, arg ) ->
                                ( n, computeV context arg )
                            )
            in
            case funcImpl of
                Just impl ->
                    computeV (computedArgs ++ context) impl

                Nothing ->
                    error ("cannot find name in module: " ++ funcName)

        FuncCall ns funcName args ->
            let
                funcImpl =
                    List.Extra.find (\( n, _ ) -> n == funcName) ns
                        |> Maybe.map Tuple.second
            in
            case funcImpl of
                Just impl ->
                    -- there's no type check here, we just populate
                    -- the namespace assuming funcImpl will ask for
                    -- the right names via VarName
                    computeV (args ++ ns) impl

                Nothing ->
                    error ("cannot find name in module: " ++ funcName)

        IfElse cond ifExpr elseExpr ->
            case getValue context cond of
                VBool b ->
                    if b then
                        computeV context ifExpr

                    else
                        computeV context elseExpr

                VError s ->
                    error ("error with if conditional: " ++ s)

                _ ->
                    error "if needs a conditional"

        Infix opLeft binOp opRight ->
            case getFuncVV context binOp of
                Ok fvv ->
                    fvv context opLeft opRight

                Err s ->
                    error ("infix needs a binary operator: " ++ s)

        _ ->
            error "cannot evaluate this type as a value yet"


getFuncV : Context -> Expr -> Result String FV
getFuncV context expr =
    case expr of
        Var _ v ->
            getFuncV context v

        FunctionV _ f ->
            Ok f

        ComposeF _ _ f ->
            Ok f

        LambdaLeft _ binOp opRight ->
            case getFuncVV context binOp of
                Ok fvv ->
                    let
                        fv c opLeft =
                            fvv c opLeft opRight
                    in
                    Ok fv

                Err s ->
                    Err ("lambda left needs a function: " ++ s)

        LambdaRight opLeft binOp _ ->
            case getFuncVV context binOp of
                Ok fvv ->
                    let
                        fv c opRight =
                            fvv c opLeft opRight
                    in
                    Ok fv

                Err s ->
                    Err ("lambda left needs a function: " ++ s)

        _ ->
            Err "not a function"


getFuncVV : Context -> Expr -> Result String FVV
getFuncVV _ expr =
    case expr of
        FunctionVV _ fvv ->
            Ok fvv

        BinOp _ fvv ->
            Ok fvv

        _ ->
            Err "not a function"


evalPipeLine : Context -> Expr -> List Expr -> Expr
evalPipeLine context v lst =
    case lst of
        [] ->
            computeV context v

        head :: rest ->
            case getFuncV context head of
                Ok f ->
                    let
                        newV =
                            f context v
                    in
                    evalPipeLine context newV rest

                Err s ->
                    error ("wanted function in pipeline: " ++ s)


getFinalValue : Expr -> V
getFinalValue expr =
    case expr of
        ComputedValue v ->
            v

        SimpleValue v ->
            v

        _ ->
            VError "final values were never computed with computeVal"
