module Enum exposing
    ( Enum, create, fromIterator
    , EnumInt, createInt, fromIntIterator
    )

{-|


# String-based Enums

@docs Enum, create, fromIterator


# Int-based Enums

@docs EnumInt, createInt, fromIntIterator

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


iterate : (a -> ( b, a )) -> a -> List ( b, a )
iterate iterator init =
    let
        helper : a -> List ( b, a ) -> List ( b, a )
        helper prevValue stack =
            let
                item =
                    iterator prevValue

                value =
                    Tuple.second item
            in
            if value == init then
                -- we get our starting value last, so we first reverse the list
                -- we built, and then add the starting value to the front.
                -- If the order is not actually important, we can also just
                -- return `item :: stack` here
                item :: List.reverse stack

            else
                helper value (item :: stack)
    in
    helper init []
