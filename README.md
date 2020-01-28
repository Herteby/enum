# Enum

When dealing with enums there are a handful of functions which you end up writing again and again. This package creates them all for you in one go.

```
import Enum

type Fruit
    = Apple
    | Banana
    | Mango

fruit : Enum Fruit
fruit =
    Enum.create
        [ ( "Apple", Apple )
        , ( "Banana", Banana )
        , ( "Mango", Mango )
        ]
```
This gives you an `Enum` which is a record containing common helper functions:
```
type alias Enum a =
    { toString : a -> String
    , fromString : String -> Maybe a
    , encode : a -> Value
    , decoder : Decoder a
    , list : List ( String, a )
    }
```
Which you can then use like:
```
fruitListDecoder : Decoder (List Fruit)
fruitListDecoder = Decode.list fruit.decoder

encodeFruitList : List Fruit -> Value
encodeFruitList = Encode.list fruit.encode
```
There is also `createInt` for enums based on `Int` instead of `String`

## Code Generator
Writing enum definitions can become tiring, especially if they change often.
[This web based code generator](https://herteby.github.io/enum/generator) lets you just paste a list of names, and it will generate the type definition and `Enum.create` code for you!

## Enum.fromIterator

One issue with enums is that when you add another variant, the compiler won't tell you if you forgot to update your `decoder` etc. to include it. The same goes for the list you provide to `Enum.create`.

To help with this, this package includes an alternate way to create enums:

```
type Fruit
    = Apple
    | Banana
    | Mango

fruit : Enum Fruit
fruit =
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
```
The end result is exactly the same as with `Enum.create`, but the `case..of` helps ensure that you never forget to add a variant!
There's also a corresponding `Enum.fromIntIterator`.