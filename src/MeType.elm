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
    | VarName String
    | Var String Expr
    | Call String Context
    | FuncCall Context String Context
    | FunctionV String FV
    | ComposeF String Expr FV
    | BinOp String FVV
    | FunctionVV String FVV
    | PipeLine Expr (List Expr)
    | Infix Expr Expr Expr
    | LambdaLeft String Expr Expr
    | LambdaRight Expr Expr String
    | IfElse Expr Expr Expr
    | LetIn Context Expr


type alias Context =
    List ( String, Expr )
