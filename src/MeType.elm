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
    | FuncCall Context String Context
    | FunctionV String FV
    | ComposeF String Expr FV
    | BinOp String FVV
    | FunctionVV String FVV
    | PipeLine Expr (List Expr)
    | LambdaLeft String Expr Expr
    | LambdaRight Expr Expr String
    | LetIn Context Expr


type alias Context =
    List ( String, Expr )
