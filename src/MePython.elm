module MePython exposing
    ( fromContext
    , prelude
    , toPython
    )

import Dict
import MeRepr
import MeType
    exposing
        ( Context
        , Expr(..)
        , V(..)
        )


prelude : String
prelude =
    """
from Kernel import (
    toElm,
    toPy,
    )
from Elm import F, pipe
import Basics
import List
import Tuple

"""


toPyWrap : String -> String
toPyWrap s =
    "toPy(" ++ s ++ ")"


toElmWrap : String -> String
toElmWrap s =
    "toElm(" ++ s ++ ")"


opName : Expr -> String
opName expr =
    case expr of
        OpFunc _ _ name ->
            name

        _ ->
            "???"


def : String -> List String -> Expr -> String
def name params expr =
    let
        topLine =
            "def "
                ++ name
                ++ "("
                ++ (params |> String.join ", ")
                ++ "):\n"

        body =
            case expr of
                LetIn _ _ ->
                    expr |> toPython |> indent

                _ ->
                    "    return \\\n"
                        ++ (expr |> toPython |> indent |> indent)
    in
    topLine ++ body


block : ( String, Expr ) -> String
block ( name, expr ) =
    case expr of
        F1 name1 lambdaExpr ->
            def name [ name1 ] lambdaExpr

        F2 name1 name2 lambdaExpr ->
            def name [ name1, name2 ] lambdaExpr

        F3 name1 name2 name3 lambdaExpr ->
            def name [ name1, name2, name3 ] lambdaExpr

        F4 name1 name2 name3 name4 lambdaExpr ->
            def name [ name1, name2, name3, name4 ] lambdaExpr

        F5 name1 name2 name3 name4 name5 lambdaExpr ->
            def name [ name1, name2, name3, name4, name5 ] lambdaExpr

        _ ->
            name
                ++ " = \\\n"
                ++ (expr |> toPython |> indent)


formatLets : List ( String, Expr ) -> String
formatLets lets =
    lets
        |> List.map block
        |> String.join "\n\n"


fromContext : Context -> String
fromContext ns =
    ns
        |> Dict.toList
        |> List.map block
        |> String.join "\n\n"


parens : String -> String
parens s =
    "(" ++ s ++ ")"


indent : String -> String
indent code =
    code
        |> String.split "\n"
        |> List.map (\s -> "    " ++ s)
        |> String.join "\n"


fWrap : String -> String
fWrap s =
    "F(" ++ s ++ ")"


aN : List Expr -> String
aN args =
    case args of
        [] ->
            "unexpected"

        head :: rest ->
            (head |> toPython |> fWrap)
                ++ (rest |> argList)


argList : List Expr -> String
argList args =
    "(\n"
        ++ (args |> List.map toPython |> String.join ",\n" |> indent)
        ++ "\n)"


fN : List String -> Expr -> String
fN params expr =
    "lambda "
        ++ (params |> String.join ", ")
        ++ ":\n"
        ++ (expr |> toPython |> indent)
        ++ "\n"


toPython : Expr -> String
toPython expr =
    case expr of
        Var name _ ->
            name

        VarName name ->
            name

        NamedFunc name _ ->
            name

        OpFunc _ _ name ->
            if name == "::" then
                "List.cons"

            else
                "lambda a, b: a "
                    ++ name
                    ++ " b"

        SimpleValue v ->
            case v of
                VInt n ->
                    expr |> MeRepr.fromExpr

                VFloat n ->
                    expr |> MeRepr.fromExpr

                _ ->
                    MeRepr.fromV v toPython
                        |> toElmWrap

        Infix argLeft opExpr argRight ->
            let
                left =
                    argLeft |> toPython

                op =
                    opExpr |> opName

                right =
                    argRight |> toPython
            in
            case op of
                "::" ->
                    "List.cons("
                        ++ left
                        ++ ", "
                        ++ right
                        ++ ")"

                "++" ->
                    "List.append("
                        ++ left
                        ++ ", "
                        ++ right
                        ++ ")"

                _ ->
                    (left
                        ++ " "
                        ++ op
                        ++ " "
                        ++ right
                    )
                        |> toElmWrap

        LetIn lets vexpr ->
            formatLets lets ++ "\n\nreturn " ++ toPython vexpr

        IfElse cond expr1 expr2 ->
            "("
                ++ (expr1 |> toPython)
                ++ "\nif\n"
                ++ (cond |> toPython |> toPyWrap |> indent)
                ++ "\nelse\n"
                ++ (toPython expr2 |> indent)
                ++ ")"

        Call funcName args ->
            (funcName |> parens)
                ++ (args |> argList)

        PipeLine a lst ->
            "pipe("
                ++ (a |> toPython)
                ++ ",\n[\n"
                ++ (lst
                        |> List.map toPython
                        |> String.join ",\n"
                        |> indent
                   )
                ++ "\n])\n"

        A1 f arg1 ->
            [ f, arg1 ]
                |> aN

        A2 f arg1 arg2 ->
            [ f, arg1, arg2 ]
                |> aN

        A3 f arg1 arg2 arg3 ->
            [ f, arg1, arg2, arg3 ]
                |> aN

        A4 f arg1 arg2 arg3 arg4 ->
            [ f, arg1, arg2, arg3, arg4 ]
                |> aN

        A5 f arg1 arg2 arg3 arg4 arg5 ->
            [ f, arg1, arg2, arg3, arg4, arg5 ]
                |> aN

        F1 name1 lambdaExpr ->
            fN [ name1 ] lambdaExpr

        F2 name1 name2 lambdaExpr ->
            fN [ name1, name2 ] lambdaExpr

        F3 name1 name2 name3 lambdaExpr ->
            fN [ name1, name2, name3 ] lambdaExpr

        F4 name1 name2 name3 name4 lambdaExpr ->
            fN [ name1, name2, name3, name4 ] lambdaExpr

        F5 name1 name2 name3 name4 name5 lambdaExpr ->
            fN [ name1, name2, name3, name4, name5 ] lambdaExpr

        _ ->
            "(not supported)"
