module MeType exposing
    ( Context
    , Expr(..)
    , FV
    , FVV
    , V(..)
    )


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
    = SimpleValue V
    | ComputedValue V
    | ComputedFunc FV
    | NamedFunc String FV
    | VarName String
    | Var String Expr
    | Call String Context
    | FuncCall Context String Context
    | BinOp String FVV
    | PipeLine Expr (List Expr)
    | F1 Expr Expr
    | F2 Expr Expr Expr
    | Infix Expr Expr Expr
    | LambdaLeft String Expr Expr
    | LambdaRight Expr Expr String
    | IfElse Expr Expr Expr
    | LetIn Context Expr


type alias Context =
    List ( String, Expr )
