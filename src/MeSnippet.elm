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


isEven : Expr
isEven =
    F1 "n" <|
        Infix
            (A2 MeInt.modBy (MeInt.init 2) (VarName "n"))
            MeInt.eq
            (MeInt.init 0)


partition : Expr
partition =
    Function [ "lst" ] <|
        A2 MeList.partition isEven (VarName "lst")


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
                , A1 MeList.map
                    (F1 "n" (Infix (VarName "n") MeNumber.plus (MeFloat.init 0.5)))
                , F1 "items" (Infix (MeFloat.init 0.5) MeList.cons (VarName "items"))
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
                    (F1 "x" (Infix (VarName "startList") MeList.append (VarName "x")))
                ]
            )


normalize : Expr
normalize =
    let
        nPlusOne =
            F1 "n"
                (Infix (VarName "n") MeNumber.plus (MeInt.init 1))
    in
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ A1 MeList.indexedMap MeTuple.pair
            , A1 MeList.sortBy MeTuple.second
            , A1 MeList.map MeTuple.first
            , A1 MeList.indexedMap MeTuple.pair
            , A1 MeList.sortBy MeTuple.second
            , A1 MeList.map MeTuple.first
            , A1 MeList.map nPlusOne
            ]


isEmpty : Expr
isEmpty =
    Function [ "lst" ] <|
        A1 MeList.isEmpty (VarName "lst")


length : Expr
length =
    Function [ "lst" ] <|
        A1 MeList.length (VarName "lst")


intersperse : Expr
intersperse =
    Function [ "n" ] <|
        A2 MeList.intersperse (VarName "n") (MeList.initInts [ 1, 2, 3, 4 ])


incr : Expr
incr =
    Function [ "n" ] <|
        A1
            (F1 "x" (Infix (VarName "x") MeNumber.plus (MeInt.init 1)))
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
            [ A1 MeList.filter (F1 "x" (Infix (VarName "x") MeInt.eq (MeInt.init 4))) ]


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
            (F1 "x" (Infix (MeInt.init 4) MeInt.eq (VarName "x")))
            (VarName "lst")


all : Expr
all =
    Function [ "lst" ] <|
        A2
            MeList.all
            (F1 "x" (Infix (MeInt.init 1) MeInt.eq (VarName "x")))
            (VarName "lst")


head : Expr
head =
    Function [ "lst" ] <|
        A1 MeList.head (VarName "lst")


tail : Expr
tail =
    Function [ "lst" ] <|
        A1 MeList.tail (VarName "lst")


concat : Expr
concat =
    Function [ "lst" ] <|
        A1 MeList.concat (VarName "lst")


concatMap : Expr
concatMap =
    Function [ "lst" ] <|
        A2 MeList.concatMap
            (A1 MeList.map (F1 "n" (Infix (VarName "n") MeNumber.mult (MeInt.init 5))))
            (VarName "lst")


xyz : Expr
xyz =
    F3 "x" "y" "z" <|
        Infix
            (Infix (VarName "x") MeNumber.mult (VarName "y"))
            MeNumber.plus
            (VarName "z")


map3 : Expr
map3 =
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ A3
                MeList.map3
                xyz
                (MeList.initInts [ 10, 20, 30 ])
                (MeList.initInts [ 5, 8, 7 ])
            ]


map2 : Expr
map2 =
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ A2 MeList.map2 MeList.range (MeList.initInts [ 1, 2, 3 ]) ]


pythag : Expr
pythag =
    F2 "x" "y" <|
        Infix
            (Infix (VarName "x") MeNumber.mult (VarName "x"))
            MeNumber.plus
            (Infix (VarName "y") MeNumber.mult (VarName "y"))


map2Pythag : Expr
map2Pythag =
    Function [ "lst" ] <|
        PipeLine
            (VarName "lst")
            [ A2 MeList.map2 pythag (MeList.initInts [ 3, 5, 7 ]) ]


drop : Expr
drop =
    Function [ "lst" ] <|
        A2 MeList.drop (MeInt.init 2) (VarName "lst")


take : Expr
take =
    Function [ "lst" ] <|
        A2 MeList.take (MeInt.init 2) (VarName "lst")


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
    , helper all "all" "[1, 1, 3]"
    , helper any "any" "[1, 2, 3]"
    , helper any "any" "[1, 4, 3]"
    , helper basicTupleStuff "basicTupleStuff" "5"
    , helper basicListStuff "basicListStuff" "5"
    , helper concat "concat" "[ [1,2,3], [4,5,6], [7,8]]"
    , helper concatMap "concatMap" "[ [1,2,3], [4,5,6], [7,8]]"
    , helper drop "drop" "[]"
    , helper drop "drop" "[1, 2, 3]"
    , helper factorial "factorial" "17"
    , helper factorial2 "factorial2" "11"
    , helper filter "filter" "[ 4, 1, 2, 3, 4, 7, 4 ]"
    , helper filterMap "filterMap" "[ [1], [], [2], [], [3] ]"
    , helper foldr "foldr" "[ 1, 2, 3]"
    , helper head "head" "[]"
    , helper head "head" "[1, 2, 3]"
    , helper incr "incr" "8"
    , helper intersperse "intersperse" "999"
    , helper isEmpty "isEmpty" "[]"
    , helper isEmpty "isEmpty" "[1, 2]"
    , helper length "length" "[1, 2, 3]"
    , helper map2 "map2" "[8, 7, 9]"
    , helper map2Pythag "map2Pythag" "[4, 12, 24]"
    , helper map3 "map3" "[8, 7, 9]"
    , helper maximum "maximum" "[]"
    , helper maximum "maximum" "[40, 10, 30, 20]"
    , helper member "member" "[41, 42, 43]"
    , helper minimum "minimum" "[]"
    , helper minimum "minimum" "[40, 10, 30, 20]"
    , helper normalize "normalize" "[ 40, 31, 59, 12, 27 ]"
    , helper partition "partition" "[1, 2, 3, 4, 5, 6, 7]"
    , helper permuteFloats "permuteFloats" "[ 4, 3, 2, 5, 1 ]"
    , helper product "product" "[1.2, 2.3, 3.8]"
    , helper repeat "repeat" "5"
    , helper reverse "reverse" "[1, 2, 3]"
    , helper sum "sum" "[1.2, 2.3, 3.8]"
    , helper tail "tail" "[]"
    , helper tail "tail" "[1, 2, 3]"
    , helper take "take" "[]"
    , helper take "take" "[1, 2, 3]"
    ]
