module MeSnippet exposing (testData)

import Dict exposing (Dict)
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
        ( Context
        , Expr(..)
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
            (F2 MeList.range (MeInt.init 1) (VarName "n"))
            [ F2 MeList.foldl MeNumber.mult (MeInt.init 1)
            ]


permuteFloats : Expr
permuteFloats =
    let
        startList =
            PipeLine
                (VarName "lst")
                [ F1 MeList.map MeInt.toFloat
                ]

        newElements =
            PipeLine
                (VarName "startList")
                [ MeList.sortFloat
                , F1 MeList.map (LambdaLeft "n" MeNumber.plus (MeFloat.init 0.5))
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
                [ F1 MeList.map MeList.singleton
                , F1 MeList.map
                    (LambdaRight (VarName "startList") MeList.plus "x")
                ]
            )


normalize : Expr
normalize =
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ F1 MeList.indexedMap MeTuple.pair
            , F1 MeList.sortByInt MeTuple.second
            , F1 MeList.map MeTuple.first
            , F1 MeList.indexedMap MeTuple.pair
            , F1 MeList.sortByInt MeTuple.second
            , F1 MeList.map MeTuple.first
            , F1 MeList.map (LambdaLeft "n" MeNumber.plus (MeInt.init 1))
            ]


helper : Expr -> String -> String -> String -> List String
helper f funcName argName inString =
    let
        ns =
            Dict.fromList [ ( funcName, f ) ]

        inExpr =
            inString
                |> MeParser.toExpr

        code =
            ns
                |> MeElmCode.codeFromContext

        args =
            [ inExpr ]

        outString =
            FuncCall ns funcName args
                |> MeRunTime.computeExpr
                |> MeRepr.fromExpr
    in
    [ code
    , inString
    , outString
    ]


testData : List (List String)
testData =
    [ helper factorial "factorial" "n" "17"
    , helper factorial2 "factorial2" "n" "11"
    , helper normalize "normalize" "lst" "[ 99, 98, 97, 100, 101, 44, 42, 41 ]"
    , helper permuteFloats "permuteFloats" "lst" "[ 4, 3, 2, 5, 1 ]"
    ]
