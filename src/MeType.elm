module MeType exposing
    ( Expr(..)
    , V(..)
    , FV, FVV, Context
    )

{-| The main type here is `Expr`. In MetaElm almost
every piece of code is represented as an `Expr`.


# Expr and V

@docs Expr

@docs V


# Helper types

@docs FV, FVV, Context

-}

import Dict exposing (Dict)


{-| primitives and basic containers
-}
type V
    = VBool Bool
    | VInt Int
    | VFloat Float
    | VTuple ( Expr, Expr )
    | VList (List Expr)
    | VError String


{-| function w/one var
-}
type alias FV =
    Context -> Expr -> Expr


{-| function w/two vars
-}
type alias FVV =
    Context -> Expr -> Expr -> Expr


{-| AST node for MetaElm code
-}
type Expr
    = A1 Expr Expr
    | A2 Expr Expr Expr
    | BinOp String FVV
    | Call String (List Expr)
    | ComputedFunc FV
    | ComputedValue V
    | FuncCall Context String (List Expr)
    | Function (List String) Expr
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


{-| namespace (maps names to expressions)
-}
type alias Context =
    Dict String Expr
