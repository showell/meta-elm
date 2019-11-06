module MeElmCode exposing
    ( codeFromContext
    , toElmCode
    )

{--

Given an Expr written for the ME runtime, this
generates the equivalent Elm code.

This code is not meant to be executed--it's meant
more for documentation.  To actually evaluate the
Expr, use the ME runtime.

Of course, nothing is stopping you from running the
Elm code that gets generated here, but that would
require some tooling.
--}

import Dict exposing (Dict)
import MeRepr
import MeType exposing (..)


indent : String -> String
indent code =
    code
        |> String.split "\n"
        |> List.map (\s -> "    " ++ s)
        |> String.join "\n"


codeFromContext : Context -> String
codeFromContext ns =
    let
        block ( name, expr ) =
            name
                ++ paramString expr
                ++ " =\n"
                ++ (indent <| toElmCode expr)
    in
    ns
        |> Dict.toList
        |> List.map block
        |> String.join "\n\n"


paramString : Expr -> String
paramString expr =
    case expr of
        Function params _ ->
            " " ++ String.join " " params

        _ ->
            ""


toElmCode : Expr -> String
toElmCode topExpr =
    let
        withParens s =
            "(" ++ s ++ ")"

        withoutParens s =
            s

        toCode parenWrapper expr =
            case expr of
                Function _ fexpr ->
                    toCode withoutParens fexpr

                LetIn lets vexpr ->
                    let
                        assign ( name, v ) =
                            name
                                ++ " =\n"
                                ++ (toElmCode v |> indent)

                        letBody =
                            lets
                                |> List.map assign
                                |> String.join "\n\n"

                        result =
                            toCode withoutParens vexpr
                    in
                    "let\n"
                        ++ indent letBody
                        ++ "\nin\n"
                        ++ result

                IfElse cond expr1 expr2 ->
                    "if "
                        ++ toCode withoutParens cond
                        ++ " then\n"
                        ++ (indent <| toCode withoutParens expr1)
                        ++ "\nelse\n"
                        ++ (indent <| toCode withoutParens expr2)

                Var name _ ->
                    -- TODO: make let statements
                    name

                VarName name ->
                    name

                Call funcName args ->
                    let
                        argString =
                            args
                                |> List.map (toCode withParens)
                                |> String.join " "
                    in
                    funcName
                        ++ " "
                        ++ argString
                        |> parenWrapper

                FuncCall _ _ _ ->
                    "(not implemented)"

                SimpleValue _ ->
                    MeRepr.fromExpr expr

                PipeLine a lst ->
                    a
                        :: lst
                        |> List.map (toCode withoutParens)
                        |> String.join "\n    |> "

                BinOp opname _ ->
                    "(" ++ opname ++ ")"

                NamedFunc name _ ->
                    name

                F1 f arg1 ->
                    toCode withParens f
                        ++ " "
                        ++ toCode withParens arg1
                        |> parenWrapper

                F2 f arg1 arg2 ->
                    toCode withParens f
                        ++ " "
                        ++ toCode withParens arg1
                        ++ " "
                        ++ toCode withParens arg2
                        |> parenWrapper

                Infix argLeft opExpr argRight ->
                    case opExpr of
                        BinOp opName _ ->
                            toCode withParens argLeft
                                ++ " "
                                ++ opName
                                ++ " "
                                ++ toCode withParens argRight
                                |> parenWrapper

                        _ ->
                            "?"

                LambdaRight argLeft opExpr vname ->
                    case opExpr of
                        BinOp opName _ ->
                            "\\"
                                ++ vname
                                ++ " -> "
                                ++ toCode withParens argLeft
                                ++ " "
                                ++ opName
                                ++ " "
                                ++ vname
                                |> parenWrapper

                        _ ->
                            "?"

                LambdaLeft vname opExpr argRight ->
                    case opExpr of
                        BinOp opName _ ->
                            "\\"
                                ++ vname
                                ++ " -> "
                                ++ vname
                                ++ " "
                                ++ opName
                                ++ " "
                                ++ toCode withParens argRight
                                |> parenWrapper

                        _ ->
                            "?"

                _ ->
                    "?"
    in
    toCode withoutParens topExpr
