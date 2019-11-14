module MeSnippet exposing (testData)

import Dict
import MeCodeGen
import MeElmCode
import MeFloat
import MeInt
import MeList
import MeNumber
import MeParser
import MeRepr
import MeRunTime
import MeTuple
import MeType
    exposing
        ( Expr(..)
        , V(..)
        )


factorial : Expr
factorial =
    Function [ "n" ] <|
        IfElse
            (Infix (VarName "n") MeInt.eq (MeInt.init 0))
            (MeInt.init 1)
            (Infix
                (VarName "n")
                MeNumber.mult
                (Call "factorial" <|
                    [ Infix (VarName "n") MeNumber.minus (MeInt.init 1)
                    ]
                )
            )


factorial2 : Expr
factorial2 =
    Function [ "n" ] <|
        PipeLine
            (A2 MeList.range (MeInt.init 1) (VarName "n"))
            [ A2 MeList.foldl MeNumber.mult (MeInt.init 1)
            ]


permuteFloats : Expr
permuteFloats =
    let
        startList =
            PipeLine
                (VarName "lst")
                [ A1 MeList.map MeInt.toFloat
                ]

        newElements =
            PipeLine
                (VarName "startList")
                [ MeList.sort
                , A1 MeList.map (LambdaLeft "n" MeNumber.plus (MeFloat.init 0.5))
                , LambdaRight (MeFloat.init 0.5) MeList.cons "items"
                ]
    in
    Function [ "lst" ] <|
        LetIn
            [ ( "startList", startList )
            , ( "newElements", newElements )
            ]
            (PipeLine
                (VarName "newElements")
                [ A1 MeList.map MeList.singleton
                , A1 MeList.map
                    (LambdaRight (VarName "startList") MeList.append "x")
                ]
            )


normalize : Expr
normalize =
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ A1 MeList.indexedMap MeTuple.pair
            , A1 MeList.sortBy MeTuple.second
            , A1 MeList.map MeTuple.first
            , A1 MeList.indexedMap MeTuple.pair
            , A1 MeList.sortBy MeTuple.second
            , A1 MeList.map MeTuple.first
            , A1 MeList.map (LambdaLeft "n" MeNumber.plus (MeInt.init 1))
            ]


isEmpty : Expr
isEmpty =
    Function [ "lst" ] <|
        A1 MeList.isEmpty (VarName "lst")


length : Expr
length =
    Function [ "lst" ] <|
        A1 MeList.length (VarName "lst")


incr : Expr
incr =
    Function [ "n" ] <|
        A1
            (LambdaLeft "x" MeNumber.plus (MeInt.init 1))
            (VarName "n")


basicTupleStuff : Expr
basicTupleStuff =
    Function [ "n" ]
        (( MeInt.init 5, Infix (MeInt.init 2) MeNumber.plus (MeInt.init 4) )
            |> VTuple
            |> SimpleValue
        )


basicListStuff : Expr
basicListStuff =
    Function [ "n" ]
        ([ MeInt.init 5
         , Infix (MeInt.init 2) MeNumber.plus (MeInt.init 4)
         , Infix (VarName "n") MeNumber.plus (MeInt.init 100)
         ]
            |> VList
            |> SimpleValue
        )


repeat : Expr
repeat =
    Function [ "n" ] <|
        A2 MeList.repeat (VarName "n") (MeInt.init 42)


foldr : Expr
foldr =
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ A2 MeList.foldr MeList.cons (MeList.initInts []) ]


filter : Expr
filter =
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ A1 MeList.filter (LambdaLeft "x" MeInt.eq (MeInt.init 4)) ]


reverse : Expr
reverse =
    Function [ "lst" ] <|
        A1 MeList.reverse (VarName "lst")


member : Expr
member =
    Function [ "lst" ] <|
        A2 MeList.member (MeInt.init 42) (VarName "lst")


any : Expr
any =
    Function [ "lst" ] <|
        A2
            MeList.any
            (LambdaRight (MeInt.init 4) MeInt.eq "x")
            (VarName "lst")


all : Expr
all =
    Function [ "lst" ] <|
        A2
            MeList.all
            (LambdaRight (MeInt.init 1) MeInt.eq "x")
            (VarName "lst")


head : Expr
head =
    Function [ "lst" ] <|
        A1 MeList.head (VarName "lst")


maximum : Expr
maximum =
    Function [ "lst" ] <|
        A1 MeList.maximum (VarName "lst")


minimum : Expr
minimum =
    Function [ "lst" ] <|
        A1 MeList.minimum (VarName "lst")


product : Expr
product =
    Function [ "lst" ] <|
        A1 MeList.product (VarName "lst")


sum : Expr
sum =
    Function [ "lst" ] <|
        A1 MeList.sum (VarName "lst")


filterMap : Expr
filterMap =
    Function [ "lst" ] <|
        A2 MeList.filterMap MeList.head (VarName "lst")


helper : Expr -> String -> String -> List String
helper f funcName inString =
    let
        ns =
            Dict.fromList [ ( funcName, f ) ]

        inExpr =
            inString
                |> MeParser.toExpr

        code =
            ns
                |> MeElmCode.codeFromContext

        code2 =
            ns
                |> MeCodeGen.fromContext

        args =
            [ inExpr ]

        outString =
            FuncCall ns funcName args
                |> MeRunTime.computeExpr
                |> MeRepr.fromExpr
    in
    [ code
    , code2
    , inString
    , outString
    ]


testData : List (List String)
testData =
    [ helper all "all" "[1, 1, 1]"
    , helper any "any" "[1, 2, 3]"
    , helper basicTupleStuff "basicTupleStuff" "5"
    , helper basicListStuff "basicListStuff" "5"
    , helper factorial "factorial" "17"
    , helper factorial2 "factorial2" "11"
    , helper filter "filter" "[ 4, 1, 2, 3, 4, 7, 4 ]"
    , helper filterMap "filterMap" "[ [1], [], [2], [], [3] ]"
    , helper foldr "foldr" "[ 1, 2, 3]"
    , helper head "head" "[]"
    , helper head "head" "[1, 2, 3]"
    , helper incr "incr" "8"
    , helper isEmpty "isEmpty" "[]"
    , helper isEmpty "isEmpty" "[1, 2]"
    , helper length "length" "[1, 2, 3]"
    , helper maximum "maximum" "[]"
    , helper maximum "maximum" "[40, 10, 30, 20]"
    , helper member "member" "[41, 42, 43]"
    , helper minimum "minimum" "[]"
    , helper minimum "minimum" "[40, 10, 30, 20]"
    , helper normalize "normalize" "[ 40, 31, 59, 12, 27 ]"
    , helper permuteFloats "permuteFloats" "[ 4, 3, 2, 5, 1 ]"
    , helper product "product" "[1.2, 2.3, 3.8]"
    , helper repeat "repeat" "5"
    , helper reverse "reverse" "[1, 2, 3]"
    , helper sum "sum" "[1.2, 2.3, 3.8]"
    ]
