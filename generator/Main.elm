module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List
import Regex
import String.Extra as String


type alias Model =
    { text : String }


initialModel : Model
initialModel =
    { text = "Apple, Banana, Mango" }


type Msg
    = Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input str ->
            { model | text = str }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ div [ class "main" ]
                [ h1 []
                    [ text "Code generator for "
                    , a [ href "https://package.elm-lang.org/packages/Herteby/enum/latest/" ] [ text "Herteby/enum" ]
                    ]
                ]
            ]
        , div [ class "main" ]
            [ h3 [] [ text "Enter a list of names" ]
            , textarea [ onInput Input, value model.text, autofocus True ] []
            , h3 [] [ text "Generated code" ]
            , pre []
                [ text (generate model.text)
                ]
            , node "style" [] [ text css ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


generate : String -> String
generate str =
    let
        rgx =
            Regex.fromString "[a-zA-Z0-9_]+" |> Maybe.withDefault Regex.never

        list =
            Regex.find rgx str |> List.map .match

        typedef =
            "type Foo\n    = " ++ String.join "\n    | " (List.map String.toSentenceCase list)

        tuples =
            list
                |> List.map (\item -> "(\"" ++ item ++ "\", " ++ String.toSentenceCase item ++ ")")
                |> String.join "\n        , "
    in
    if list == [] then
        "Please enter a list of names"

    else if List.length (List.uniqueBy String.toSentenceCase list) /= List.length list then
        "The names are not unique"

    else
        typedef ++ "\n\n\n" ++ "enum : Enum Foo\nenum =\n    Enum.create\n        [ " ++ tuples ++ "\n        ]"


css =
    """
body {
  height:100%;
  font-family:sans-serif;
}
h1,h2,h3,h4,h5,h6,h7{
  font-weight:normal;
}
.header {
  background:#1293D8;
  color:white;
  padding: 1px;
}
.header a {
  color:white;
}
.main {
  max-width: 800px;
  margin:auto;
  padding:0 20px;
}
textarea{
  width:100%;
  box-sizing:border-box;
  height:100px;
  border:1px solid lightgrey;
  padding:10px;
  resize:none;
}
pre {
  border:1px solid lightgrey;
  padding:10px;
  user-select:all;
}
"""
