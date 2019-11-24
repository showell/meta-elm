module MeExample exposing (view)

import Html exposing (Html)
import Html.Attributes
    exposing
        ( style
        )
import List.Extra
import MeSnippet
import MeType exposing (Expr(..), V(..))
import MeWrapper


view : List (Html msg)
view =
    pythonCode



-- testResults


pythonCode : List (Html msg)
pythonCode =
    MeSnippet.testData
        |> List.map (List.Extra.getAt 1)
        |> List.map (Maybe.withDefault "??")
        |> String.join "\n\n"
        |> Html.text
        |> List.singleton
        |> Html.pre []
        |> List.singleton


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
            , "python"
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
