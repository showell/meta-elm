module MeType exposing
    ( Expr(..)
    , V(..)
    , FV, FVV, FVVV, FVVVV, FVVVVV, Context
    )

{-| The main type here is `Expr`. In MetaElm almost
every piece of code is represented as an `Expr`.


# Expr and V

@docs Expr

@docs V


# Helper types

@docs FV, FVV, FVVV, FVVVV, FVVVVV, Context

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
    | VMaybe (Maybe Expr)


{-| function w/one var
-}
type alias FV =
    Context -> Expr -> Expr


{-| function w/two vars
-}
type alias FVV =
    Context -> Expr -> Expr -> Expr


{-| function w/three vars
-}
type alias FVVV =
    Context -> Expr -> Expr -> Expr -> Expr


{-| function w/four vars
-}
type alias FVVVV =
    Context -> Expr -> Expr -> Expr -> Expr -> Expr


{-| function w/five vars
-}
type alias FVVVVV =
    Context -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr


{-| AST node for MetaElm code
-}
type Expr
    = A1 Expr Expr
    | A2 Expr Expr Expr
    | A3 Expr Expr Expr Expr
    | A4 Expr Expr Expr Expr Expr
    | A5 Expr Expr Expr Expr Expr Expr
    | Call String (List Expr)
    | ComputedFunc FV
    | ComputedValue V
    | F1 String Expr
    | F2 String String Expr
    | F3 String String String Expr
    | F4 String String String String Expr
    | F5 String String String String String Expr
    | FuncCall Context String (List Expr)
    | IfElse Expr Expr Expr
    | Infix Expr Expr Expr
    | LetIn (List ( String, Expr )) Expr
    | NamedFunc String FV
    | OpFunc String FV String
    | PipeLine Expr (List Expr)
    | SimpleValue V
    | Var String Expr
    | VarName String


{-| namespace (maps names to expressions)
-}
type alias Context =
    Dict String Expr
