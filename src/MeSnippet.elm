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
        ( Context
        , Expr(..)
        )


factorial : Context
factorial =
    let
        f =
            IfElse
                (Infix (VarName "n") MeInt.eq (MeInt.init 0))
                (MeInt.init 1)
                (Infix
                    (VarName "n")
                    MeNumber.mult
                    (Call "factorial"
                        [ ( "n", Infix (VarName "n") MeNumber.minus (MeInt.init 1) )
                        ]
                    )
                )
    in
    [ ( "factorial", f ) ]


permuteFloats : Context
permuteFloats =
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
    [ ( "permuteFloats", f ) ]


normalize : Context
normalize =
    let
        f =
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
    [ ( "normalize", f ) ]


helper : Context -> String -> String -> String -> List String
helper ns funcName argName inString =
    let
        inExpr =
            inString
                |> MeParser.toExpr

        code =
            ns
                |> MeElmCode.codeFromContext

        outString =
            FuncCall ns funcName [ ( argName, inExpr ) ]
                |> MeRunTime.computeVal
                |> MeRepr.fromVal
    in
    [ code
    , inString
    , outString
    ]


testData : List (List String)
testData =
    [ helper factorial "factorial" "n" "17"
    , helper normalize "normalize" "lst" "[ 99, 98, 97, 100, 101, 44, 42, 41 ]"
    , helper permuteFloats "permuteFloats" "lst" "[ 4, 3, 2, 5, 1 ]"
    ]
