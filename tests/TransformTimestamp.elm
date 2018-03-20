module TransformTimestamp exposing (..)

import Dict
import Expect
import Test exposing (Test, describe, test)
import View.Charts exposing (transformTimestamp)


missingValueTests : Test
missingValueTests =
    describe "Key not found in dict"
        [ test "not found"
            (\_ ->
                Expect.equal Dict.empty (transformTimestamp "not found" Dict.empty)
            )
        , test "not a date gives back empty"
            (\_ ->
                let
                    input =
                        Dict.insert "a key" "not a timestamp" Dict.empty
                in
                Expect.equal Dict.empty (transformTimestamp "a key" input)
            )
        ]


parsesFullIsoFormat : Test
parsesFullIsoFormat =
    describe "ISO Format comes through fine"
        [ test "iso in UTC"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "2018-01-01T10:10:10+00:00" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T10:10:10.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        , test "iso in UTC using Z specifier"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "2018-01-01T10:10:10.000Z" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T10:10:10.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        , test "iso not in UTC"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "2018-01-01T10:10:10.000+08:00" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T02:10:10.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        , test "Will allow space character instead of 'T'"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "2018-01-01 10:10:10.000Z" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T10:10:10.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        ]


assumesUTCIfNotSpecified : Test
assumesUTCIfNotSpecified =
    describe "Adds UTC offset when needed"
        [ test "Full date time with no TZ"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "2018-01-01T10:10:10" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T10:10:10.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        , test "Full date no time, no TZ"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "2018-01-01" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T00:00:00.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        , test "Will allow space character instead of 'T'"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "2018-01-01 10:10:10.000" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T10:10:10.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        ]


parsesSlashedFormat : Test
parsesSlashedFormat =
    describe "Parses date in slashed format like m/d/y"
        [ test "m/d/y date with no time"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "1/1/2018" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T00:00:00.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        , test "date with time ignores time part"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "1/1/2018 14:12:12" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T00:00:00.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        , test "date with time and zone ignores time part"
            (\_ ->
                let
                    input =
                        Dict.insert "timestamp" "1/1/2018 10:10:10.000+10:00" Dict.empty

                    expected =
                        Dict.insert "timestamp" "2018-01-01T00:00:00.000Z" Dict.empty
                in
                Expect.equal expected (transformTimestamp "timestamp" input)
            )
        ]
