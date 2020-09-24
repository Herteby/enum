module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List
import Regex exposing (Regex)
import String.Extra as String


type alias Model =
    { name : String, variants : String }


initialModel : Model
initialModel =
    { name = "Fruit", variants = "Apple Banana Mango" }


type Msg
    = SetName String
    | SetVariants String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetName str ->
            { model | name = str }

        SetVariants str ->
            { model | variants = str }


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
            [ h3 [] [ text "Enter a name for the type" ]
            , input [ onInput SetName, value model.name, autofocus True ] []
            , h3 [] [ text "Enter a list of variants ", small [] [ text "(all characters except alphanumerics and underscore count as separators)" ] ]
            , textarea [ onInput SetVariants, value model.variants ] []
            , h3 [] [ text "Generated code" ]
            , case generate model of
                Ok code ->
                    pre [] [ text code ]

                Err message ->
                    pre [ class "error" ] [ text message ]
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


generate : Model -> Result String String
generate model =
    let
        list =
            model.variants
                |> Regex.find (rgx "[a-zA-Z0-9_]+")
                |> List.map .match

        typedef =
            "type " ++ String.toSentenceCase model.name ++ "\n    = " ++ String.join "\n    | " (List.map String.toSentenceCase list)

        tuples =
            list
                |> List.map
                    (\item -> "(\"" ++ item ++ "\", " ++ String.toSentenceCase item ++ ")")
                |> String.join "\n        , "
    in
    if model.name == "" then
        Err "Please give a name for the type"

    else if Regex.contains (rgx "[^a-zA-Z0-9_]") model.name then
        Err "The name contains invalid characters"

    else if not <| Regex.contains (rgx "^[a-zA-Z]") model.name then
        Err "The name must start with a letter"

    else if List.any (not << Regex.contains (rgx "^[a-zA-Z]")) list then
        Err "Variant names must start with a letter"

    else if list == [] then
        Err "Please enter a list of variants"

    else if List.length (List.uniqueBy String.toSentenceCase list) /= List.length list then
        Err "The variants are not unique"

    else
        Ok <| typedef ++ "\n\n\n" ++ "enum : Enum " ++ String.toSentenceCase model.name ++ "\nenum =\n    Enum.create\n        [ " ++ tuples ++ "\n        ]"


rgx : String -> Regex
rgx =
    Regex.fromString >> Maybe.withDefault Regex.never


css : String
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
  background:#5FABDC;
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
input, textarea{
  width:100%;
  box-sizing:border-box;
  border:1px solid lightgrey;
  padding:10px;
}
textarea {
  height:100px;
  resize:none;
}
pre {
  border:1px solid lightgrey;
  padding:10px;
}
.error{
    color:red;
    border-color:red;
}
small {
  color:#888;
}
"""
