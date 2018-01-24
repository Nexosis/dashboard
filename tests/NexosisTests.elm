module NexosisTests exposing (..)

import Test exposing (Test, describe, test)
import Expect
import Data.DataSet exposing (decodeDataSetList)


tests : Test
tests =
    describe "Basic test"
        [ test "Placeholder" <|
            \() ->
                Expect.equal (3 + 7) 11
        ]
