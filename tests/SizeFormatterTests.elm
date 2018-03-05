module SizeFormatterTests exposing (..)

import Test exposing (Test, describe, test)
import Expect
import Util exposing (dataSizeWithSuffix)

suite : Test
suite = 
    describe "formatter output"
        [test "formatter handles zero"
            (\_ -> Expect.equal " - " (dataSizeWithSuffix 0) )
        , test "formatter uses B below 1024"
            (\_ -> Expect.equal "100 B" (dataSizeWithSuffix 100) )
        , test "formatter switches to Kb below 10mb"
            (\_ -> Expect.equal "15 Kb" (dataSizeWithSuffix 15360) )
        , test "formatter switches to Mb above 10mb"
            (\_ -> Expect.equal "10 Mb" (dataSizeWithSuffix 10485761) )
        , test "formatter uses comma when over 3 digits"
            (\_ -> Expect.equal "1,024 Kb" (dataSizeWithSuffix 1048576) )
        ]