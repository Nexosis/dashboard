module TransformTimestamp exposing (..)

import Date
import Date.Extra as Date
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
        , test "not a date gives back epoch"
            (\_ ->
                let
                    input =
                        Dict.insert "a key" "not a timestamp" Dict.empty

                    expected =
                        Dict.insert "a key" (0 |> Date.fromTime |> Date.toUtcIsoString) Dict.empty
                in
                Expect.equal expected (transformTimestamp "a key" input)
            )
        ]
