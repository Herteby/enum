module Enum exposing
    ( Enum, create, fromIterator
    , EnumInt, createInt, fromIndex, fromIntIterator, fromIterator2
    )

{-|


# String-based Enums

@docs Enum, create, fromIterator


# Int-based Enums

@docs EnumInt, createInt, fromIndex, fromIntIterator, fromIterator2

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


{-| An `Enum` is a record containing common helper functions
-}
type alias Enum a =
    { toString : a -> String
    , fromString : String -> Maybe a
    , encode : a -> Value
    , decoder : Decoder a
    , dict : Dict String a
    , list : List ( String, a )
    }


{-| Create an `Enum` from a list of `String`s and variants

    type Fruit
        = Apple
        | Banana
        | Mango

    enum : Enum Fruit
    enum =
        Enum.create
            [ ( "Apple", Apple )
            , ( "Banana", Banana )
            , ( "Mango", Mango )
            ]

-}
create : List ( String, a ) -> Enum a
create list =
    let
        dict =
            Dict.fromList list

        toString a =
            case list |> List.filter (\( str, b ) -> b == a) |> List.head of
                Just ( str, b ) ->
                    str

                Nothing ->
                    "Missing enum"

        fromString string =
            Dict.get string dict
    in
    { toString = toString
    , fromString = fromString
    , encode = toString >> Encode.string
    , decoder =
        Decode.string
            |> Decode.andThen
                (\string ->
                    case Dict.get string dict of
                        Just a ->
                            Decode.succeed a

                        Nothing ->
                            Decode.fail ("Missing enum: " ++ string)
                )
    , dict = dict
    , list = list
    }


{-| Create an `Enum` from an iterator function.
It may look unusual, but the benefit is that the `case` statement helps ensure that you don't forget a variant.

    type Fruit
        = Apple
        | Banana
        | Mango

    enum : Enum Fruit
    enum =
        Enum.fromIterator
            (\a ->
                case a of
                    Apple ->
                        ( "Banana", Banana )

                    Banana ->
                        ( "Mango", Mango )

                    Mango ->
                        ( "Apple", Apple )
            )
            Apple

-}
fromIterator : (a -> ( String, a )) -> a -> Enum a
fromIterator iterator start =
    create (iterate iterator start)


{-| Enums based on `Int`s instead of `String`s
-}
type alias EnumInt a =
    { toInt : a -> Int
    , fromInt : Int -> Maybe a
    , encode : a -> Value
    , decoder : Decoder a
    , dict : Dict Int a
    , list : List ( Int, a )
    }


{-| Create an `EnumInt` from a list of `Int`s and variants

    type Fruit
        = Apple
        | Banana
        | Mango

    enum : EnumInt Fruit
    enum =
        Enum.createInt
            [ ( 1, Apple )
            , ( 2, Banana )
            , ( 3, Mango )
            ]

-}
createInt : List ( Int, a ) -> EnumInt a
createInt list =
    let
        dict =
            Dict.fromList list

        toInt a =
            case list |> List.filter (\( int, b ) -> b == a) |> List.head of
                Just ( int, b ) ->
                    int

                Nothing ->
                    -1

        fromInt int =
            Dict.get int dict
    in
    { toInt = toInt
    , fromInt = fromInt
    , encode = toInt >> Encode.int
    , decoder =
        Decode.int
            |> Decode.andThen
                (\int ->
                    case Dict.get int dict of
                        Just a ->
                            Decode.succeed a

                        Nothing ->
                            Decode.fail ("Missing enum: " ++ String.fromInt int)
                )
    , dict = dict
    , list = list
    }


{-| Create an `EnumInt` from an iterator function

    type Fruit
        = Apple
        | Banana
        | Mango

    enum : EnumInt Fruit
    enum =
        Enum.fromIntIterator
            (\a ->
                case a of
                    Mango ->
                        ( 1, Apple )

                    Apple ->
                        ( 2, Banana )

                    Banana ->
                        ( 3, Mango )
            )
            Apple

-}
fromIntIterator : (a -> ( Int, a )) -> a -> EnumInt a
fromIntIterator iterator start =
    createInt (iterate iterator start)


{-| Create an `EnumInt` based on list index

    type Fruit
        = Apple
        | Banana
        | Mango

    enum : EnumInt Fruit
    enum =
        Enum.fromIndex
            [ Apple
            , Banana
            , Mango
            ]

-}
fromIndex : List a -> EnumInt a
fromIndex =
    List.indexedMap Tuple.pair >> createInt


{-| Create an `EnumInt` from an iterator

    type Fruit
        = Apple
        | Banana
        | Mango

    enum : EnumInt Fruit
    enum =
        Enum.fromIterator2
            (\a ->
                case a of
                    Apple ->
                        Banana

                    Banana ->
                        Mango

                    Mango ->
                        Apple
            )
            Apple

-}
fromIterator2 : (a -> a) -> a -> EnumInt a
fromIterator2 iterator start =
    iterate2 iterator start
        |> List.indexedMap Tuple.pair
        |> createInt


iterate : (a -> ( b, a )) -> a -> List ( b, a )
iterate iterator first =
    let
        helper : a -> List ( b, a )
        helper current =
            let
                next =
                    iterator current
            in
            if Tuple.second next == first then
                [ next ]

            else
                helper (Tuple.second next) ++ [ next ]

        list =
            helper first

        head =
            list |> List.head |> Maybe.map List.singleton |> Maybe.withDefault []

        tail =
            helper first |> List.drop 1 |> List.reverse
    in
    head ++ tail


iterate2 : (a -> a) -> a -> List a
iterate2 iterator first =
    let
        helper : a -> List a
        helper current =
            if iterator current == first then
                current :: []

            else
                current :: helper (iterator current)
    in
    helper first
