module Tests exposing (suite)

import Dict
import Enum exposing (Enum, EnumInt)
import Expect exposing (Expectation)
import Json.Decode as Decode exposing (decodeString, decodeValue)
import Json.Encode as Encode
import Test exposing (..)



-- FIXTURES


type Fruit
    = Apple
    | Banana
    | Mango


fruits : List Fruit
fruits =
    [ Apple, Banana, Mango ]


strings : List String
strings =
    [ "Apple", "Banana", "Mango" ]


stringTuples : List ( String, Fruit )
stringTuples =
    List.map2 Tuple.pair strings fruits


ints : List Int
ints =
    [ 1, 2, 3 ]


intTuples : List ( Int, Fruit )
intTuples =
    List.map2 Tuple.pair ints fruits


stringIterator fruit =
    case fruit of
        Apple ->
            ( "Banana", Banana )

        Banana ->
            ( "Mango", Mango )

        Mango ->
            ( "Apple", Apple )


intIterator fruit =
    case fruit of
        Mango ->
            ( 1, Apple )

        Apple ->
            ( 2, Banana )

        Banana ->
            ( 3, Mango )



-- TEST CREATORS


stringTests : Enum Fruit -> List Test
stringTests enum =
    [ test "toString" <|
        \_ -> fruits |> List.map enum.toString |> Expect.equal strings
    , test "fromString" <|
        \_ -> strings |> List.map enum.fromString |> Expect.equal (List.map Just fruits)
    , test "decoder" <|
        \_ -> """["Apple","Banana","Mango"]""" |> decodeString (Decode.list enum.decoder) |> Expect.equal (Ok fruits)
    , test "encode" <|
        \_ -> fruits |> Encode.list enum.encode |> decodeValue (Decode.list enum.decoder) |> Expect.equal (Ok fruits)
    , test "dict" <|
        \_ -> enum.dict |> Expect.equal (Dict.fromList stringTuples)
    , test "list" <|
        \_ -> enum.list |> Expect.equal stringTuples
    ]


intTests : EnumInt Fruit -> List Test
intTests enum =
    [ test "toInt" <|
        \_ -> fruits |> List.map enum.toInt |> Expect.equal ints
    , test "fromInt" <|
        \_ -> ints |> List.map enum.fromInt |> Expect.equal (List.map Just fruits)
    , test "decoder" <|
        \_ -> """[1,2,3]""" |> decodeString (Decode.list enum.decoder) |> Expect.equal (Ok fruits)
    , test "encode" <|
        \_ -> fruits |> Encode.list enum.encode |> decodeValue (Decode.list enum.decoder) |> Expect.equal (Ok fruits)
    , test "dict" <|
        \_ -> enum.dict |> Expect.equal (Dict.fromList intTuples)
    , test "list" <|
        \_ -> enum.list |> Expect.equal intTuples
    ]



-- SUITES


suite : Test
suite =
    describe "Enum"
        [ describe "create" <| stringTests <| Enum.create stringTuples
        , describe "fromIterator" <| stringTests <| Enum.fromIterator stringIterator Apple
        , describe "createInt" <| intTests <| Enum.createInt intTuples
        , describe "fromIntIterator" <| intTests <| Enum.fromIntIterator intIterator Apple
        ]
