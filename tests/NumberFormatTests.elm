module NumberFormatTests exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Util exposing (commaFormatInteger, formatFloatToString, isActuallyInteger)


isFloatTests : Test
isFloatTests =
    describe "decimal tester"
        [ test "tester false when decimals"
            (\_ -> Expect.equal False (isActuallyInteger 0.122))
        , test "tester true when zero"
            (\_ -> Expect.equal True (isActuallyInteger 0.0))
        , test "tester true when no decimals"
            (\_ -> Expect.equal True (isActuallyInteger 232.0))
        , test "false even if very small"
            (\_ -> Expect.equal False (isActuallyInteger 0.000000000000001))
        ]


floatFormatterTests : Test
floatFormatterTests =
    describe "float formatter"
        [ test "returns integer format when not decimal"
            (\_ -> Expect.equal "15" (formatFloatToString 15.0))
        , test "returns all five decimals"
            (\_ -> Expect.equal "15.12345" (formatFloatToString 15.12345))
        , test "returns only five decimals rounded"
            (\_ -> Expect.equal "15.12346" (formatFloatToString 15.123456789))
        , test "returns zero with five decimals"
            (\_ -> Expect.equal "0.12345" (formatFloatToString 0.12345))
        , test "returns zero as just 0"
            (\_ -> Expect.equal "0" (formatFloatToString 0.0))
        ]


commaFormatterTests : Test
commaFormatterTests =
    describe "integer formatter"
        [ test "returns with comma when larger than 999"
            (\_ -> Expect.equal "1,000" (commaFormatInteger 1000))
        , test
            "returns with no comma when smaller than 1000"
            (\_ -> Expect.equal "999" (commaFormatInteger 999))
        , test
            "handles multiple commas for really large numbers"
            (\_ -> Expect.equal "999,999,999" (commaFormatInteger 999999999))
        ]
