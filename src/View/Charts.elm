module View.Charts exposing (forecastResults)

import Data.Columns as Columns exposing (ColumnMetadata)
import Data.Session as Session exposing (SessionData, SessionResults)
import Dict exposing (Dict)
import List.Extra exposing (find)
import VegaLite exposing (..)


forecastResults : SessionResults -> SessionData -> Int -> Spec
forecastResults sessionResults session windowWidth =
    let
        targetColumn =
            session.columns
                |> find (\c -> c.role == Columns.Target)

        timestampColumn =
            session.columns
                |> find (\c -> c.role == Columns.Timestamp)

        ( chartWidth, chartHeight ) =
            widthToSize windowWidth
    in
    case Maybe.map2 (,) targetColumn timestampColumn of
        Just ( targetCol, timestampCol ) ->
            let
                enc =
                    encoding
                        << position X [ PName timestampCol.name, PmType Temporal, PTimeUnit <| resultIntervalToTimeUnit session.resultInterval ]
                        << position Y [ PName targetCol.name, PmType Quantitative ]
            in
            toVegaLite
                [ title "Results"
                , width chartWidth
                , height chartHeight
                , autosize [ AFit, APadding ]
                , dataFromRows [] <| List.concatMap resultsToRows sessionResults.data
                , mark Line []
                , enc []
                ]

        _ ->
            toVegaLite
                []


resultsToRows : Dict String String -> List DataRow
resultsToRows result =
    dataRow
        (result
            |> Dict.toList
            |> List.map (\( k, v ) -> ( k, Str v ))
        )
        []


resultIntervalToTimeUnit : Maybe Session.ResultInterval -> TimeUnit
resultIntervalToTimeUnit resultInterval =
    case resultInterval of
        Just Session.Hour ->
            Hours

        _ ->
            YearMonthDate


widthToSize : Int -> ( Float, Float )
widthToSize width =
    if width >= 1440 then
        ( 1140, 570 )
    else if width >= 768 then
        ( 768, 384 )
    else
        ( 429, 215 )
