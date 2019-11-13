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
                    (LambdaRight (VarName "startList") MeList.plus "x")
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
    [ helper basicTupleStuff "basicTupleStuff" "5"
    , helper basicListStuff "basicListStuff" "5"
    , helper incr "incr" "8"
    , helper factorial "factorial" "17"
    , helper factorial2 "factorial2" "11"
    , helper normalize "normalize" "[ 40, 31, 59, 12, 27 ]"
    , helper permuteFloats "permuteFloats" "[ 4, 3, 2, 5, 1 ]"
    ]
