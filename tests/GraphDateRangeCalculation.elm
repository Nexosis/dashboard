module GraphDateRangeCalculation exposing (..)

import Data.PredictionDomain as PredictionDomain
import Data.Session exposing (ResultInterval(..))
import Expect
import Page.SessionDetail exposing (SessionDateData, getDataDateRange)
import Test exposing (Test, describe, test)


nothingsReturnEmptyStrings : Test
nothingsReturnEmptyStrings =
    describe "nothings tests"
        [ test "returns tuple of empty strings when both start/end are Nothing"
            (\_ ->
                Expect.equal ( "", "" ) (getDataDateRange (SessionDateData Nothing Nothing Nothing PredictionDomain.Forecast))
            )
        , test "returns tuple of empty strings when start is Nothing"
            (\_ ->
                Expect.equal ( "", "" ) (getDataDateRange (SessionDateData Nothing (Just "2018-01-10") Nothing PredictionDomain.Forecast))
            )
        , test "returns tuple of empty strings when end is Nothing"
            (\_ ->
                Expect.equal ( "", "" ) (getDataDateRange (SessionDateData (Just "2018-01-10") Nothing Nothing PredictionDomain.Forecast))
            )
        ]


impactSessionDates : Test
impactSessionDates =
    describe "impact sessions date ranges"
        [ test "returns range with daily count before and after"
            (\_ ->
                Expect.equal ( "2018-02-15T00:00:00.000Z", "2018-03-29T00:00:00.000Z" ) (getDataDateRange (SessionDateData (Just "2018-03-01T00:00:00.000Z") (Just "2018-03-15T00:00:00.000Z") (Just Day) PredictionDomain.Impact))
            )
        , test "returns range with hourly count before and after"
            (\_ ->
                Expect.equal ( "2018-02-28T09:00:00.000Z", "2018-03-02T06:00:00.000Z" ) (getDataDateRange (SessionDateData (Just "2018-03-01T00:00:00.000Z") (Just "2018-03-01T15:00:00.000Z") (Just Hour) PredictionDomain.Impact))
            )
        , test "returns range with weekly count before and after"
            (\_ ->
                Expect.equal ( "2018-02-01T00:00:00.000Z", "2018-04-26T00:00:00.000Z" ) (getDataDateRange (SessionDateData (Just "2018-03-01T00:00:00.000Z") (Just "2018-03-29T00:00:00.000Z") (Just Week) PredictionDomain.Impact))
            )
        ]


forecastSessionDates : Test
forecastSessionDates =
    describe "forecast sessions date ranges"
        [ test "returns range with daily 2x count before"
            (\_ ->
                Expect.equal ( "2018-02-01T00:00:00.000Z", "2018-03-15T00:00:00.000Z" ) (getDataDateRange (SessionDateData (Just "2018-03-01T00:00:00.000Z") (Just "2018-03-15T00:00:00.000Z") (Just Day) PredictionDomain.Forecast))
            )
        , test "returns range with hourly 2x count before"
            (\_ ->
                Expect.equal ( "2018-02-27T18:00:00.000Z", "2018-03-01T15:00:00.000Z" ) (getDataDateRange (SessionDateData (Just "2018-03-01T00:00:00.000Z") (Just "2018-03-01T15:00:00.000Z") (Just Hour) PredictionDomain.Forecast))
            )
        ]
