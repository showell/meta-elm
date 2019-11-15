module MeCodeGen exposing (toString, fromContext)

{-| generate code from MetaElm Expr values
(using Elm.CodeGen to produce the string output)

@docs toString, fromContext

-}

import Dict
import Elm.CodeGen as CG
import Elm.Pretty
import Elm.Syntax.Expression
import MeRepr
import MeType
    exposing
        ( Context
        , Expr(..)
        , V(..)
        )
import Pretty


pretty x =
    -- this is narrow due to how I'm displaying them now
    -- a more typical value is 120
    x |> Pretty.pretty 45


{-| produce code from Dict of name -> expression
-}
fromContext : Context -> String
fromContext ns =
    let
        paramPatterns expr =
            case expr of
                Function params _ ->
                    params
                        |> List.map CG.varPattern

                _ ->
                    []

        block ( name, expr ) =
            CG.funDecl
                Nothing
                Nothing
                name
                (paramPatterns expr)
                (expr |> toCG)
                |> Elm.Pretty.prettyDeclaration
                |> pretty
    in
    ns
        |> Dict.toList
        |> List.map block
        |> String.join "\n\n"


letDecl : ( String, Expr ) -> Elm.Syntax.Expression.LetDeclaration
letDecl ( name, expr ) =
    CG.letVal
        name
        (expr |> toCG)


formatCode : CG.Expression -> String
formatCode code =
    code
        |> Elm.Pretty.prettyExpression
        |> pretty


binOp expr1 op expr2 =
    -- CG.infixNon may be deprecated eventually
    CG.opApply op CG.infixNon expr1 expr2


{-| turn Expr into Elm code (w/pretty printing)
-}
toString : Expr -> String
toString expr =
    expr
        |> toCG
        |> formatCode


toCG : Expr -> CG.Expression
toCG expr =
    case expr of
        Var name _ ->
            name |> CG.val

        VarName name ->
            name |> CG.val

        NamedFunc name _ ->
            name |> CG.val

        OpFunc name _ _ ->
            name |> CG.val

        Function _ fexpr ->
            fexpr |> toCG

        LetIn lets vexpr ->
            CG.letExpr
                (lets |> List.map letDecl)
                (vexpr |> toCG)

        IfElse cond expr1 expr2 ->
            CG.ifExpr
                (cond |> toCG)
                (expr1 |> toCG)
                (expr2 |> toCG)

        Call funcName args ->
            (funcName |> CG.fun)
                :: (args |> List.map toCG)
                |> CG.apply

        SimpleValue v ->
            case v of
                VList lst ->
                    lst
                        |> List.map toCG
                        |> CG.list

                VTuple ( a, b ) ->
                    [ a |> toCG
                    , b |> toCG
                    ]
                        |> CG.tuple

                _ ->
                    expr
                        |> MeRepr.fromExpr
                        |> CG.val

        PipeLine a lst ->
            CG.pipe
                (a |> toCG)
                (lst |> List.map toCG)

        A1 f arg1 ->
            [ f, arg1 ]
                |> List.map toCG
                |> CG.apply

        A2 f arg1 arg2 ->
            [ f, arg1, arg2 ]
                |> List.map toCG
                |> CG.apply

        A3 f arg1 arg2 arg3 ->
            [ f, arg1, arg2, arg3 ]
                |> List.map toCG
                |> CG.apply

        F1 name1 lambdaExpr ->
            CG.lambda
                ([ name1 ]
                    |> List.map CG.varPattern
                )
                (lambdaExpr |> toCG)

        F3 name1 name2 name3 lambdaExpr ->
            CG.lambda
                ([ name1, name2, name3 ]
                    |> List.map CG.varPattern
                )
                (lambdaExpr |> toCG)

        BinOp opname _ ->
            opname
                |> (\s -> "(" ++ s ++ ")")
                |> CG.val

        Infix argLeft opExpr argRight ->
            case opExpr of
                BinOp opName _ ->
                    binOp
                        (argLeft |> toCG)
                        opName
                        (argRight |> toCG)
                        |> CG.parens

                OpFunc _ _ opName ->
                    binOp
                        (argLeft |> toCG)
                        opName
                        (argRight |> toCG)
                        |> CG.parens

                _ ->
                    CG.val "?"

        FuncCall _ _ _ ->
            CG.val "?"

        ComputedFunc _ ->
            CG.val "?"

        ComputedValue _ ->
            CG.val "?"
