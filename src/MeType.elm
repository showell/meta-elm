module MeType exposing
    ( Context
    , Expr(..)
    , FV
    , FVV
    , V(..)
    )


type V
    = VInt Int
    | VFloat Float
    | VTuple ( Expr, Expr )
    | VList (List Expr)
    | VError String


type alias FV =
    Context -> Expr -> V


type alias FVV =
    Context -> Expr -> Expr -> V


type Expr
    = SimpleValue V
    | ComputedValue V
    | VarName String
    | Var String Expr
    | UserFunction String (List String) Expr
    | FunctionCall Expr Context
    | FunctionV String FV
    | ComposeF String Expr FV
    | FunctionVV String FVV
    | PipeLine Expr (List Expr)
    | Curry Expr Expr


type alias Context =
    List ( String, Expr )
