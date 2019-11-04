module MeSnippet exposing (testData)

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
        )


permuteFloats : Expr -> Expr
permuteFloats testList =
    let
        startList =
            PipeLine (VarName "lst") [ MeList.map MeInt.toFloat ]

        newElements =
            PipeLine
                (VarName "startList")
                [ MeList.sortFloat
                , MeList.map <| LambdaLeft "n" MeNumber.plus (MeFloat.init 0.5)
                , LambdaRight (MeFloat.init 0.5) MeList.cons "items"
                ]

        f =
            UserFunction "permuteFloats" [ "lst" ] <|
                LetIn
                    [ ( "startList", startList )
                    , ( "newElements", newElements )
                    ]
                    (PipeLine
                        (VarName "newElements")
                        [ MeList.map MeList.singleton
                        , MeList.map
                            (LambdaRight (VarName "startList") MeList.plus "x")
                        ]
                    )
    in
    FunctionCall f
        [ ( "lst", testList )
        ]


normalize : Expr -> Expr
normalize testList =
    let
        f =
            UserFunction "normalize" [ "lst" ] <|
                PipeLine
                    (VarName "lst")
                    [ MeList.indexedMap MeTuple.pair
                    , MeList.sortByInt MeTuple.second
                    , MeList.map MeTuple.first
                    , MeList.indexedMap MeTuple.pair
                    , MeList.sortByInt MeTuple.second
                    , MeList.map MeTuple.first
                    , MeList.map <| LambdaLeft "n" MeNumber.plus (MeInt.init 1)
                    ]
    in
    FunctionCall f
        [ ( "lst", testList )
        ]


helper : (Expr -> Expr) -> String -> List String
helper f inData =
    let
        inExpr =
            inData
                |> MeParser.toExpr

        code =
            inExpr
                |> f
                |> MeElmCode.toElmCode

        outVal =
            inExpr
                |> f
                |> MeRunTime.computeVal
                |> MeRepr.fromVal
    in
    [ code
    , inData
    , outVal
    ]


testData : List (List String)
testData =
    [ helper normalize "[ 99, 98, 97, 100, 101, 44, 42, 41 ]"
    , helper permuteFloats "[ 4, 3, 2, 5, 1 ]"
    ]
