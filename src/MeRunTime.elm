module MeRunTime exposing
    ( computeV
    , computeVal
    , getFinalValue
    , getFuncV
    , getFuncVV
    )

import List.Extra
import MeType exposing (..)


computeVal : Expr -> V
computeVal expr =
    let
        context =
            []
    in
    computeV context expr


computeV : FV
computeV context expr =
    case expr of
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
                    VError ("cannot find " ++ vname)

        ComputedValue v ->
            v

        SimpleValue v ->
            v

        PipeLine topExpr lst ->
            evalPipeLine context topExpr lst

        FunctionCall calledFunc argTups ->
            -- we are assuming for now that we don't have
            -- vars in outer functions or at module scope
            case calledFunc of
                UserFunction fName params f ->
                    let
                        args =
                            argTups
                                |> List.map Tuple.first
                    in
                    if args == params then
                        computeV argTups f

                    else
                        VError ("args do not match params for " ++ fName)

                _ ->
                    VError "only user functions are supported now"

        _ ->
            VError "cannot evaluate this type as a value yet"


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


evalPipeLine : Context -> Expr -> List Expr -> V
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
                    evalPipeLine context (ComputedValue newV) rest

                Err s ->
                    VError ("wanted function in pipeline: " ++ s)


getFinalValue : Expr -> Result String V
getFinalValue expr =
    case expr of
        ComputedValue v ->
            Ok v

        SimpleValue v ->
            Ok v

        _ ->
            Err "final values were never computed with computeVal"
