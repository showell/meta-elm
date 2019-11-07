module MeCodeGen exposing (toString)

import Elm.CodeGen as CG
import Elm.Pretty
import Elm.Syntax.Expression
import MeRepr
import MeType
    exposing
        ( Expr(..)
        )
import Pretty


{-| use Elm.Pretty to print out code
in nicely formatted style
-}
formatCode : CG.Expression -> String
formatCode code =
    code
        |> Elm.Pretty.prettyExpression
        |> Pretty.pretty 35


binOp expr1 op expr2 =
    -- CG.infixNon may be deprecated eventually
    CG.opApply op CG.infixNon expr1 expr2


toString : Expr -> String
toString expr =
    expr
        |> toCG
        |> formatCode


letDecl : ( String, Expr ) -> Elm.Syntax.Expression.LetDeclaration
letDecl ( name, expr ) =
    CG.letVal
        name
        (expr |> toCG)


toCG : Expr -> CG.Expression
toCG expr =
    case expr of
        Var name _ ->
            name |> CG.val

        VarName name ->
            name |> CG.val

        NamedFunc name _ ->
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
            (CG.fun funcName :: List.map toCG args)
                |> CG.apply

        SimpleValue _ ->
            MeRepr.fromExpr expr
                |> CG.val

        PipeLine a lst ->
            CG.pipe
                (a |> toCG)
                (lst |> List.map toCG)

        F1 f arg1 ->
            [ f, arg1 ]
                |> List.map toCG
                |> CG.apply

        F2 f arg1 arg2 ->
            [ f, arg1, arg2 ]
                |> List.map toCG
                |> CG.apply

        BinOp opname _ ->
            opname
                |> CG.val
                |> CG.parens

        Infix argLeft opExpr argRight ->
            case opExpr of
                BinOp opName _ ->
                    binOp
                        (argLeft |> toCG)
                        opName
                        (argRight |> toCG)
                        |> CG.parens

                _ ->
                    CG.val "?"

        LambdaRight argLeft opExpr vname ->
            case opExpr of
                BinOp opName _ ->
                    binOp
                        (argLeft |> toCG)
                        opName
                        (vname |> CG.val)
                        |> CG.lambda [ CG.varPattern vname ]

                _ ->
                    CG.val "?"

        LambdaLeft vname opExpr argRight ->
            case opExpr of
                BinOp opName _ ->
                    binOp
                        (vname |> CG.val)
                        opName
                        (argRight |> toCG)
                        |> CG.lambda [ CG.varPattern vname ]

                _ ->
                    CG.val "?"

        FuncCall _ _ _ ->
            CG.val "?"

        ComputedFunc _ ->
            CG.val "?"

        ComputedValue _ ->
            CG.val "?"