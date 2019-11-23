module MePython exposing
    ( fromContext
    , toPython
    )

import Dict
import MeRepr
import MeType
    exposing
        ( Context
        , Expr(..)
        )


op : Expr -> String
op expr =
    case expr of
        OpFunc _ _ name ->
            name

        _ ->
            "???"


def : String -> List String -> Expr -> String
def name params expr =
    "def "
        ++ name
        ++ "("
        ++ (params |> String.join ", ")
        ++ "):\n    return \\\n"
        ++ (expr |> toPython |> indent |> indent)


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
                ++ " =\n"
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
                ++ "(\n"
                ++ (rest |> List.map toPython |> String.join ",\n" |> indent)
                ++ "\n)"


fN : List String -> Expr -> String
fN params expr =
    "F(lambda "
        ++ (params |> String.join ", ")
        ++ ":\n"
        ++ (expr |> toPython |> indent)
        ++ "\n)"


toPython : Expr -> String
toPython expr =
    case expr of
        Var name _ ->
            name

        VarName name ->
            name

        NamedFunc name _ ->
            name

        OpFunc name _ _ ->
            name

        SimpleValue v ->
            MeRepr.fromV v toPython

        Infix argLeft opExpr argRight ->
            (argLeft |> toPython)
                ++ " "
                ++ (opExpr |> op)
                ++ " "
                ++ (argRight |> toPython)

        LetIn lets vexpr ->
            formatLets lets ++ "\n\nreturn " ++ toPython vexpr

        IfElse cond expr1 expr2 ->
            (expr1 |> toPython)
                ++ " if\n"
                ++ (cond |> toPython |> indent)
                ++ "\nelse\n"
                ++ (toPython expr2 |> indent)

        Call funcName args ->
            parens funcName ++ aN args

        PipeLine a lst ->
            "Elm.pipe("
                ++ (a |> toPython)
                ++ ",\n"
                ++ (lst
                        |> List.map toPython
                        |> String.join ",\n"
                        |> indent
                   )
                ++ "\n)\n"

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
