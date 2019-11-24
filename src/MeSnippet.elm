module MeSnippet exposing (pythonCode)

import Dict
import MeBasics
import MeCodeGen
import MeFloat
import MeInt
import MeList
import MeNumber
import MeParser
import MePython
import MeRepr
import MeRunTime
import MeTuple
import MeType
    exposing
        ( Expr(..)
        , V(..)
        )


timesN : String -> Int -> Expr
timesN s n =
    Infix (VarName s) MeNumber.mult (MeInt.init n)


add : Expr -> Expr -> Expr
add e1 e2 =
    Infix e1 MeNumber.plus e2


one : Expr
one =
    MeInt.init 1


two : Expr
two =
    MeInt.init 2


compare : Expr
compare =
    F1 "n" <|
        A2 MeBasics.compare two (VarName "n")


f2 : Expr
f2 =
    F2 "x" "y" <|
        add (timesN "x" 10) (VarName "y")


f3 : Expr
f3 =
    F3 "x" "y" "z" <|
        add
            (timesN "x" 100)
            (add (timesN "y" 10) (VarName "z"))


f4 : Expr
f4 =
    F4 "a" "b" "c" "d" <|
        Infix
            (Infix (VarName "a") MeNumber.plus (VarName "b"))
            MeNumber.mult
            (Infix (VarName "c") MeNumber.plus (VarName "d"))


f5 : Expr
f5 =
    F5 "a" "b" "c" "d" "e" <|
        A2 MeTuple.pair
            (VarName "a")
            (A2 MeTuple.pair
                (VarName "b")
                (A2 MeTuple.pair
                    (VarName "c")
                    (A2 MeTuple.pair
                        (VarName "d")
                        (VarName "e")
                    )
                )
            )


a1a1f2 : Expr
a1a1f2 =
    F1 "n" <|
        A1 (A1 f2 one) (VarName "n")


a1a1a1f3 : Expr
a1a1a1f3 =
    F1 "n" <|
        A1 (A1 (A1 f3 one) two) (VarName "n")


factorial : Expr
factorial =
    F1 "n" <|
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
    F1 "n" <|
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
    F1 "lst" <|
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
    F1 "lst" <|
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
    F1 "lst" <|
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
    F1 "lst" <|
        A1 MeList.isEmpty (VarName "lst")


length : Expr
length =
    F1 "lst" <|
        A1 MeList.length (VarName "lst")


intersperse : Expr
intersperse =
    F1 "n" <|
        A2 MeList.intersperse (VarName "n") (MeList.initInts [ 1, 2, 3, 4 ])


incr : Expr
incr =
    F1 "n" <|
        A1
            (F1 "x" (Infix (VarName "x") MeNumber.plus (MeInt.init 1)))
            (VarName "n")


basicTupleStuff : Expr
basicTupleStuff =
    F1 "n"
        (( MeInt.init 5, Infix (MeInt.init 2) MeNumber.plus (MeInt.init 4) )
            |> VTuple
            |> SimpleValue
        )


basicListStuff : Expr
basicListStuff =
    F1 "n"
        ([ MeInt.init 5
         , Infix (MeInt.init 2) MeNumber.plus (MeInt.init 4)
         , Infix (VarName "n") MeNumber.plus (MeInt.init 100)
         ]
            |> VList
            |> SimpleValue
        )


repeat : Expr
repeat =
    F1 "n" <|
        A2 MeList.repeat (VarName "n") (MeInt.init 42)


foldr : Expr
foldr =
    F1 "lst" <|
        PipeLine
            (VarName "lst")
            [ A2 MeList.foldr MeList.cons (MeList.initInts []) ]


filter : Expr
filter =
    F1 "lst" <|
        PipeLine
            (VarName "lst")
            [ A1 MeList.filter (F1 "x" (Infix (VarName "x") MeInt.eq (MeInt.init 4))) ]


reverse : Expr
reverse =
    F1 "lst" <|
        A1 MeList.reverse (VarName "lst")


sortWith : Expr
sortWith =
    let
        mod10 =
            F1 "n" <|
                A2 MeInt.modBy (MeInt.init 10) (VarName "n")

        withF =
            F2 "a" "b" <|
                A2 MeBasics.compare
                    (A1 mod10 (VarName "a"))
                    (A1 mod10 (VarName "b"))
    in
    F1 "lst" <|
        A2 MeList.sortWith withF (VarName "lst")


member : Expr
member =
    F1 "lst" <|
        A2 MeList.member (MeInt.init 42) (VarName "lst")


any : Expr
any =
    F1 "lst" <|
        A2
            MeList.any
            (F1 "x" (Infix (MeInt.init 4) MeInt.eq (VarName "x")))
            (VarName "lst")


all : Expr
all =
    F1 "lst" <|
        A2
            MeList.all
            (F1 "x" (Infix (MeInt.init 1) MeInt.eq (VarName "x")))
            (VarName "lst")


head : Expr
head =
    F1 "lst" <|
        A1 MeList.head (VarName "lst")


tail : Expr
tail =
    F1 "lst" <|
        A1 MeList.tail (VarName "lst")


concat : Expr
concat =
    F1 "lst" <|
        A1 MeList.concat (VarName "lst")


concatMap : Expr
concatMap =
    F1 "lst" <|
        A2 MeList.concatMap
            (A1 MeList.map (F1 "n" (Infix (VarName "n") MeNumber.mult (MeInt.init 5))))
            (VarName "lst")


map2 : Expr
map2 =
    F1 "lst" <|
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


pythagTest : Expr
pythagTest =
    F1 "n" <|
        A2 pythag (MeInt.init 9) (VarName "n")


map2Pythag : Expr
map2Pythag =
    F1 "lst" <|
        PipeLine
            (VarName "lst")
            [ A2 MeList.map2 pythag (MeList.initInts [ 3, 5, 7 ]) ]


map3 : Expr
map3 =
    F1 "lst" <|
        PipeLine
            (VarName "lst")
            [ A3
                MeList.map3
                f3
                (MeList.initInts [ 10, 20, 30 ])
                (MeList.initInts [ 5, 8, 7 ])
            ]


f4Test : Expr
f4Test =
    F1 "n" <|
        A4 f4 (MeInt.init 1) (MeInt.init 2) (MeInt.init 3) (VarName "n")


map4 : Expr
map4 =
    F1 "lst" <|
        PipeLine
            (VarName "lst")
            [ A4
                MeList.map4
                f4
                (MeList.initInts [ 10, 20, 30 ])
                (MeList.initInts [ 5, 8, 7 ])
                (MeList.initInts [ 1, 2 ])
            ]


f5Test : Expr
f5Test =
    F1 "n" <|
        A5 f5
            (MeInt.init 1)
            (MeInt.init 2)
            (MeInt.init 3)
            (MeInt.init 4)
            (VarName "n")


map5 : Expr
map5 =
    F1 "lst" <|
        PipeLine
            (VarName "lst")
            [ A5
                MeList.map5
                f5
                (MeList.initInts [ 10, 20, 30 ])
                (MeList.initInts [ 5, 8, 7 ])
                (MeList.initInts [ 1, 2, 3, 4, 5 ])
                (MeList.initInts [ 33, 97, 103 ])
            ]


drop : Expr
drop =
    F1 "lst" <|
        A2 MeList.drop (MeInt.init 2) (VarName "lst")


take : Expr
take =
    F1 "lst" <|
        A2 MeList.take (MeInt.init 2) (VarName "lst")


unzip : Expr
unzip =
    F1 "lst" <|
        PipeLine
            (VarName "lst")
            [ A1 MeList.map
                (F1 "x"
                    (A2 MeTuple.pair
                        (VarName "x")
                        (Infix (VarName "x") MeNumber.mult (MeInt.init 3))
                    )
                )
            , MeList.unzip
            ]


maximum : Expr
maximum =
    F1 "lst" <|
        A1 MeList.maximum (VarName "lst")


minimum : Expr
minimum =
    F1 "lst" <|
        A1 MeList.minimum (VarName "lst")


product : Expr
product =
    F1 "lst" <|
        A1 MeList.product (VarName "lst")


sum : Expr
sum =
    F1 "lst" <|
        A1 MeList.sum (VarName "lst")


filterMap : Expr
filterMap =
    F1 "lst" <|
        A2 MeList.filterMap MeList.head (VarName "lst")


helper : Expr -> String -> String -> String
helper f funcName inString =
    let
        ns =
            Dict.fromList [ ( funcName, f ) ]

        inExpr =
            inString
                |> MeParser.toExpr

        code =
            ns
                |> MeCodeGen.fromContext

        pythonDef =
            ns
                |> MePython.fromContext

        args =
            [ inExpr ]

        outString =
            FuncCall ns funcName args
                |> MeRunTime.computeExpr
                |> MeRepr.fromExpr

        testCall =
            "\n\n"
                ++ "test"
                ++ ([ "'" ++ funcName ++ "'"
                    , funcName
                    , inString |> String.trim
                    , outString
                    ]
                        |> MePython.formatArgs
                   )
    in
    pythonDef ++ testCall


pythonCode : List String
pythonCode =
    [ helper a1a1f2 "a1a1f2" "2"
    , helper a1a1a1f3 "a1a1a1f3" "3"
    , helper all "all" "[1, 1, 1]"
    , helper all "all" "[1, 1, 3]"
    , helper any "any" "[1, 2, 3]"
    , helper any "any" "[1, 4, 3]"
    , helper basicTupleStuff "basicTupleStuff" "5"
    , helper basicListStuff "basicListStuff" "5"
    , helper compare "compare" "1"
    , helper compare "compare" "2"
    , helper compare "compare" "3"
    , helper concat "concat" "[ [1,2,3], [4,5,6], [7,8]]"
    , helper concatMap "concatMap" "[ [1,2,3], [4,5,6], [7,8]]"
    , helper drop "drop" "[]"
    , helper drop "drop" "[1, 2, 3]"
    , helper f4Test "f4Test" "4"
    , helper f5Test "f5Test" "5"
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
    , helper map4 "map4" "[1, 3, 19, 22]"
    , helper map5 "map5" "[5, 10, 15, 20]"
    , helper maximum "maximum" "[]"
    , helper maximum "maximum" "[40, 10, 30, 20]"
    , helper member "member" "[41, 42, 43]"
    , helper minimum "minimum" "[]"
    , helper minimum "minimum" "[40, 10, 30, 20]"
    , helper normalize "normalize" "[ 40, 31, 59, 12, 27 ]"
    , helper partition "partition" "[1, 2, 3, 4, 5, 6, 7]"
    , helper permuteFloats "permuteFloats" "[ 4, 3, 2, 5, 1 ]"
    , helper product "product" "[1.2, 2.3, 3.8]"
    , helper pythagTest "pythagTest" "40"
    , helper repeat "repeat" "5"
    , helper reverse "reverse" "[1, 2, 3]"
    , helper sortWith "sortWith" "[53, 27, 11, 49, 82]"
    , helper sum "sum" "[1.2, 2.3, 3.8]"
    , helper tail "tail" "[]"
    , helper tail "tail" "[1, 2, 3]"
    , helper take "take" "[]"
    , helper take "take" "[1, 2, 3]"
    , helper unzip "unzip" "[ 1, 2, 3, 4 ]"
    ]
