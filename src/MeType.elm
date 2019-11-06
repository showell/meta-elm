module MeType exposing
    ( Context
    , Expr(..)
    , FV
    , FVV
    , V(..)
    )

import Dict exposing (Dict)


type V
    = VBool Bool
    | VInt Int
    | VFloat Float
    | VTuple ( Expr, Expr )
    | VList (List Expr)
    | VError String


type alias FV =
    Context -> Expr -> Expr


type alias FVV =
    Context -> Expr -> Expr -> Expr


type Expr
    = BinOp String FVV
    | Call String Context
    | ComputedFunc FV
    | ComputedValue V
    | F1 Expr Expr
    | F2 Expr Expr Expr
    | FuncCall Context String Context
    | IfElse Expr Expr Expr
    | Infix Expr Expr Expr
    | LambdaLeft String Expr Expr
    | LambdaRight Expr Expr String
    | LetIn (List ( String, Expr )) Expr
    | NamedFunc String FV
    | PipeLine Expr (List Expr)
    | SimpleValue V
    | Var String Expr
    | VarName String


type alias Context =
    Dict String Expr
