module ZiplistTests exposing (..)

import Test exposing (Test, describe, test, fuzz, fuzz2)
import Fuzz exposing (..)
import Expect
import List.Extra exposing (last)
import Data.Ziplist exposing (..)


fuzzTests : Test
fuzzTests =
    describe "fuzz tests"
        [ fuzz2 int (list int) "advance moves forwards, rewind moves back" <|
            \first rest ->
                create first rest
                    |> advance
                    |> rewind
                    |> Expect.equal
                        { current = first
                        , next = rest
                        , previous = []
                        }
        , fuzz2 int (list int) "advance past the end of the list" <|
            \first rest ->
                let
                    advanceTimes =
                        (List.length rest) + 2

                    currentItem =
                        last rest
                            |> Maybe.withDefault first

                    prevItems =
                        [ first ]
                            ++ rest
                            |> List.reverse
                            |> List.tail
                            |> Maybe.withDefault []
                            |> List.reverse
                in
                    create first rest
                        |> callMany advanceTimes advance
                        |> Expect.equal
                            { previous = prevItems
                            , current = currentItem
                            , next = []
                            }
        , fuzz2 int (list int) "advance and rewind past the end of the list" <|
            \first rest ->
                let
                    advanceTimes =
                        (List.length rest) + 2
                in
                    create first rest
                        |> callMany advanceTimes advance
                        |> callMany advanceTimes rewind
                        |> Expect.equal
                            { previous = []
                            , current = first
                            , next = rest
                            }
        ]


callMany : number -> (a -> a) -> a -> a
callMany times func param =
    case times of
        0 ->
            param

        n ->
            callMany (n - 1) func (func param)
