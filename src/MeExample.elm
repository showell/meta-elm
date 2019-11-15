module MeExample exposing (view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
    exposing
        ( style
        )
import MeSnippet
import MeWrapper


view : List (Html msg)
view =
    testResults ++ MeWrapper.viewWrappers


testResults : List (Html msg)
testResults =
    let
        makeTh : String -> Html msg
        makeTh s =
            s
                |> Html.text
                |> List.singleton
                |> Html.th []

        headings : Html msg
        headings =
            [ "generated code (via CodeGen)"
            , "example input"
            , "example output"
            ]
                |> List.map makeTh
                |> Html.tr []

        makeTd : String -> Html msg
        makeTd item =
            item
                |> Html.text
                |> List.singleton
                |> Html.pre []
                |> List.singleton
                |> Html.td []

        makeTr : List String -> Html msg
        makeTr items =
            items
                |> List.map makeTd
                |> Html.tr [ style "vertical-align" "top" ]
    in
    MeSnippet.testData
        |> List.map makeTr
        |> (::) headings
        |> Html.table []
        |> List.singleton
