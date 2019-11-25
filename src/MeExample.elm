module MeExample exposing (view)

import Html exposing (Html)
import MePython
import MeSnippet
import MeType exposing (Expr(..), V(..))


view : List (Html msg)
view =
    -- testResults
    pythonCode


pyTestHelper : String
pyTestHelper =
    """
def test(funcName, f, arg, expected):
    result = f(toElm(arg))
    assert result == expected
    print('pass: ', funcName)


"""


pythonCode : List (Html msg)
pythonCode =
    let
        prelude =
            MePython.prelude

        defs =
            MeSnippet.pythonCode
                |> String.join "\n\n\n"

        fullCode =
            prelude ++ pyTestHelper ++ defs
    in
    fullCode
        |> Html.text
        |> List.singleton
        |> Html.pre []
        |> List.singleton
