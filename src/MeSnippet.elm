module MeSnippet exposing (testData)

import MeElmCode
import MeFloat
import MeInt
import MeList
import MeNumber
import MeRepr
import MeRunTime
import MeTuple
import MeType


permuteFloats : MeType.Expr -> MeType.Expr
permuteFloats testList =
    let
        lst =
            MeType.VarName "lst"

        pipeline =
            MeType.PipeLine
                lst
                [ MeList.sortInt
                , MeList.map MeInt.toFloat
                , MeList.map (MeType.Curry MeNumber.plus (MeFloat.init 0.5))
                ]

        f =
            MeType.UserFunction "permuteFloats" [ "lst" ] pipeline
    in
    MeType.FunctionCall f
        [ ( "lst", testList )
        ]


permuteFloatStrings : List String
permuteFloatStrings =
    let
        inData =
            [ 4, 3, 2, 5, 1 ]

        inExpr =
            inData
                |> MeList.initInts

        code =
            inExpr
                |> permuteFloats
                |> MeElmCode.toElmCode

        outVal =
            inExpr
                |> permuteFloats
                |> MeRunTime.computeVal
    in
    [ code
    , inExpr |> MeRepr.fromExpr
    , outVal |> MeRepr.fromVal
    ]


normalize : MeType.Expr -> MeType.Expr
normalize testList =
    let
        one =
            MeInt.init 1

        incr =
            MeType.Curry MeNumber.plus one

        lst =
            MeType.VarName "lst"

        pipeline =
            MeType.PipeLine
                lst
                [ MeList.indexedMap MeTuple.pair
                , MeList.sortByInt MeTuple.second
                , MeList.map MeTuple.first
                , MeList.indexedMap MeTuple.pair
                , MeList.sortByInt MeTuple.second
                , MeList.map MeTuple.first
                , MeList.map incr
                ]

        f =
            MeType.UserFunction "normalize" [ "lst" ] pipeline
    in
    MeType.FunctionCall f
        [ ( "lst", testList )
        ]


normalizeStrings : List String
normalizeStrings =
    let
        inData =
            [ 99, 98, 97, 100, 101, 44, 42, 41 ]

        inExpr =
            inData
                |> MeList.initInts

        code =
            inExpr
                |> normalize
                |> MeElmCode.toElmCode

        outVal =
            inExpr
                |> normalize
                |> MeRunTime.computeVal
    in
    [ code
    , inExpr |> MeRepr.fromExpr
    , outVal |> MeRepr.fromVal
    ]


testData : List (List String)
testData =
    [ normalizeStrings
    , permuteFloatStrings
    ]
