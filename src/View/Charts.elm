module View.Charts exposing (forecastResults, impactResults, regressionResults, renderConfusionMatrix)

import Array
import Data.AggregationStrategy as AggregationStrategy
import Data.Columns as Columns exposing (ColumnMetadata)
import Data.ConfusionMatrix as ConfusionMatrix exposing (ConfusionMatrix)
import Data.DataSet exposing (DataSetData)
import Data.Session as Session exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, div, h3, table, tbody, td, tr)
import Html.Attributes exposing (attribute, class, style)
import List.Extra exposing (find)
import String.Extra as String exposing (replace)
import VegaLite exposing (..)


forecastResults : SessionResults -> SessionData -> DataSetData -> Int -> Spec
forecastResults sessionResults session dataSet windowWidth =
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
                pointTypeName =
                    "Result Type"

                sessionData =
                    List.map (\dict -> Dict.insert pointTypeName "Predictions" dict) sessionResults.data

                dataSetData =
                    List.map (\dict -> Dict.insert pointTypeName "Observations" dict) dataSet.data

                enc =
                    encoding
                        << position X [ PName (normalizeFieldName timestampCol.name), PmType Temporal, PTimeUnit YearMonthDateHoursMinutes, PAxis [ AxTitle "Timestamp", AxFormat (axisLabelFormat session) ] ]
                        << position Y [ PName (normalizeFieldName targetCol.name), PmType Quantitative ]
                        << color
                            [ MName pointTypeName
                            , MmType Nominal
                            , MScale <|
                                categoricalDomainMap
                                    [ ( "Predictions", "#1F77B4" )
                                    , ( "Observations", "#04850d" )
                                    ]
                            ]

                data =
                    dataFromRows [] <| List.concatMap resultsToRows (sessionData ++ dataSetData)
            in
            toVegaLite
                [ VegaLite.title "Results"
                , VegaLite.width chartWidth
                , VegaLite.height chartHeight
                , autosize [ AFit, APadding ]
                , data
                , VegaLite.mark Line [ MInterpolate Monotone ]
                , enc []
                ]

        _ ->
            toVegaLite
                []


impactResults : SessionResults -> SessionData -> DataSetData -> Int -> Spec
impactResults sessionResults session dataSet windowWidth =
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
                lineEnc =
                    encoding
                        << position X [ PName (normalizeFieldName timestampCol.name), PmType Temporal, PTimeUnit YearMonthDateHoursMinutes, PAxis [ AxTitle "Timestamp", AxFormat (axisLabelFormat session) ] ]
                        << position Y [ PName (normalizeFieldName targetCol.name), PmType Quantitative, PAggregate <| mapAggregation targetCol.aggregation, PAxis [ AxTitle targetCol.name ] ]
                        << color
                            [ MName pointTypeName
                            , MmType Nominal
                            , MScale <|
                                categoricalDomainMap
                                    [ ( "Predictions", "#1F77B4" )
                                    , ( "Observations", "#04850d" )
                                    ]
                            ]

                minPos =
                    position X [ PName (normalizeFieldName timestampCol.name), PmType Temporal, PTimeUnit YearMonthDateHoursMinutes, PAggregate Min ]

                maxPos =
                    position X [ PName (normalizeFieldName timestampCol.name), PmType Temporal, PTimeUnit YearMonthDateHoursMinutes, PAggregate Max ]

                predictionsOnly =
                    transform << filter (FOneOf pointTypeName (Strings [ "Predictions" ])) <| []

                lineSpec =
                    asSpec
                        [ lineEnc []
                        , mark Line [ MInterpolate Monotone ]
                        , transform << filter (FOneOf pointTypeName (Strings [ "Predictions", "Observations" ])) <| []
                        ]

                minSpec =
                    asSpec
                        [ (encoding << minPos) []
                        , mark Rule []
                        , predictionsOnly
                        ]

                maxSpec =
                    asSpec
                        [ (encoding << maxPos) []
                        , mark Rule []
                        , predictionsOnly
                        ]

                boxSpec =
                    asSpec
                        [ (encoding << minPos << position X2 [ PName timestampCol.name, PmType Temporal, PTimeUnit YearMonthDateHoursMinutes, PAggregate Max ]) []
                        , mark Rect [ MFillOpacity 0.1 ]
                        , predictionsOnly
                        ]

                pointTypeName =
                    "Result Type"

                sessionData =
                    List.map (\dict -> Dict.insert pointTypeName "Predictions" dict) sessionResults.data

                dataSetData =
                    List.map (\dict -> Dict.insert pointTypeName "Observations" dict) dataSet.data
            in
            toVegaLite
                [ title (Maybe.withDefault "" (Dict.get "event" session.extraParameters) ++ " Results")
                , width chartWidth
                , height chartHeight
                , autosize [ AFit, APadding ]
                , dataFromRows [] <| List.concatMap resultsToRows (sessionData ++ dataSetData)
                , layer
                    [ lineSpec
                    , minSpec
                    , maxSpec
                    , boxSpec
                    ]
                ]

        _ ->
            toVegaLite
                []


axisLabelFormat : SessionData -> String
axisLabelFormat session =
    case session.resultInterval of
        Just Hour ->
            "%x %X"

        _ ->
            "%x"


regressionResults : SessionResults -> SessionData -> Int -> Spec
regressionResults sessionResults session windowWidth =
    let
        targetColumn =
            session.columns
                |> find (\c -> c.role == Columns.Target)

        ( chartWidth, chartHeight ) =
            widthToSize windowWidth
    in
    case targetColumn of
        Just targetCol ->
            let
                dataValues =
                    sessionResults.data
                        |> List.map (resultsToPredictedObserved targetCol.name)

                ( sumX, sumY ) =
                    List.foldl (\( x, y ) ( sx, sy ) -> ( sx + x, sy + y )) ( 0, 0 ) dataValues

                sumYSquare =
                    List.foldl (\( _, y ) s -> s + y ^ 2) 0 dataValues

                sumXY =
                    List.foldl (\( x, y ) s -> s + (x * y)) 0 dataValues

                valuesLength =
                    List.length dataValues |> toFloat

                meanX =
                    sumX / valuesLength

                meanY =
                    sumY / valuesLength

                meanXY =
                    sumXY / valuesLength

                meanYSquare =
                    sumYSquare / valuesLength

                meanSquareY =
                    meanY ^ 2

                m =
                    (meanXY - (meanX * meanY)) / (meanYSquare - meanSquareY)

                b =
                    meanX - m * meanY

                actualName =
                    targetCol.name ++ ":actual"

                pointTypeName =
                    "Result Type"

                enc =
                    encoding
                        << position Y [ PName (normalizeFieldName targetCol.name), PmType Quantitative, PAxis [ AxTitle targetCol.name ] ]
                        << position X [ PName (normalizeFieldName actualName), PmType Quantitative, PAxis [ AxTitle actualName ] ]
                        << color
                            [ MName pointTypeName
                            , MmType Nominal
                            , MScale <|
                                categoricalDomainMap
                                    [ ( "Predictions", "#1F77B4" )
                                    , ( "1:1 Baseline", "#04850d" )
                                    , ( "Regression Line", "#990000" )
                                    ]
                            ]

                lineSpec =
                    asSpec
                        [ enc []
                        , mark Line []
                        , transform << filter (FOneOf pointTypeName (Strings [ "1:1 Baseline", "Regression Line" ])) <| []
                        ]

                pointSpec =
                    asSpec
                        [ enc []
                        , mark Circle []
                        , transform << filter (FOneOf pointTypeName (Strings [ "Predictions" ])) <| []
                        ]

                resultData =
                    sessionResults.data
                        |> List.map
                            (\values ->
                                Dict.insert pointTypeName "Predictions" values
                            )

                baselineData =
                    List.map
                        (\values ->
                            let
                                actual =
                                    Dict.get actualName values
                                        |> Maybe.withDefault "0"
                            in
                            values
                                |> Dict.insert pointTypeName "1:1 Baseline"
                                |> Dict.insert targetCol.name actual
                        )
                        sessionResults.data

                regressionData =
                    sessionResults.data
                        |> List.map
                            (\values ->
                                let
                                    actual =
                                        Dict.get actualName values
                                            |> Maybe.withDefault "0"
                                            |> String.toFloat
                                            |> Result.withDefault 0
                                in
                                values
                                    |> Dict.insert pointTypeName "Regression Line"
                                    |> Dict.insert targetCol.name (toString <| m * actual + b)
                            )

                joinedData =
                    resultData ++ baselineData ++ regressionData
            in
            toVegaLite
                [ title "Results"
                , width chartWidth
                , height chartHeight
                , autosize [ AFit, APadding ]
                , dataFromRows [] <| List.concatMap resultsToRows joinedData
                , layer
                    [ pointSpec
                    , lineSpec
                    ]
                ]

        _ ->
            toVegaLite
                []


resultsToPredictedObserved : String -> Dict String String -> ( Float, Float )
resultsToPredictedObserved target result =
    let
        targetValue =
            Dict.get target result

        actualValue =
            Dict.get (target ++ ":actual") result
    in
    Maybe.map2 (,) targetValue actualValue
        |> Maybe.withDefault ( "0", "0" )
        |> Tuple.mapFirst (String.toFloat >> Result.withDefault 0)
        |> Tuple.mapSecond (String.toFloat >> Result.withDefault 0)


normalizeFieldName : String -> String
normalizeFieldName key =
    replace "." "_" key


resultsToRows : Dict String String -> List DataRow
resultsToRows result =
    dataRow
        (result
            |> Dict.toList
            |> List.map (\( k, v ) -> ( normalizeFieldName k, Str v ))
        )
        []


mapAggregation : AggregationStrategy.AggregationStrategy -> Operation
mapAggregation aggregate =
    case aggregate of
        AggregationStrategy.Sum ->
            Sum

        AggregationStrategy.Mean ->
            Mean

        AggregationStrategy.Median ->
            Median

        -- HACK: VegaLite doesn't support Mode, and it's a little weird anyhow. Faking it for now.
        AggregationStrategy.Mode ->
            Mean

        AggregationStrategy.Min ->
            Min

        AggregationStrategy.Max ->
            Max


widthToSize : Int -> ( Float, Float )
widthToSize width =
    if width >= 1440 then
        ( 1140, 570 )
    else if width >= 768 then
        ( 768, 384 )
    else
        ( 429, 215 )


renderConfusionMatrix : ConfusionMatrix -> Html msg
renderConfusionMatrix matrix =
    div [ class "row" ]
        [ div [ class "col-sm-12" ]
            [ h3 [] [ Html.text "Confusion Matrix" ]
            , table [ class "table table-bordered confusion-matrix" ]
                [ tbody []
                    (List.map (\r -> toConfusionMatrixRow matrix.classes r) (Array.toIndexedList matrix.confusionMatrix)
                        -- footer is the set of classes
                        ++ [ tr [ class "footer" ] (td [] [] :: List.map (\c -> td [] [ Html.text c ]) (Array.toList matrix.classes)) ]
                    )
                ]
            ]
        ]


toConfusionMatrixRow : Array.Array String -> ( Int, Array.Array Int ) -> Html msg
toConfusionMatrixRow classes ( rowNumber, data ) =
    let
        rowMax =
            List.maximum (Array.toList data)
                |> Maybe.withDefault 0
    in
    tr []
        -- prefix each row with the class
        (td [ class "header" ] [ Html.text (Maybe.withDefault "" (Array.get rowNumber classes)) ]
            :: List.map
                (\( index, value ) ->
                    td [ class (classFromValue rowMax value (index == rowNumber)) ] [ Html.text (toString value) ]
                )
                (Array.toIndexedList data)
        )


classFromValue : Int -> Int -> Bool -> String
classFromValue maxValue value isTarget =
    let
        scaled =
            round ((toFloat value / toFloat maxValue) * 100)
    in
    if scaled == 0 then
        if isTarget then
            "really-bad"
        else
            "neutral"
    else if scaled < 33 then
        "medium"
    else if scaled < 66 then
        "bad"
    else if scaled <= 100 then
        if isTarget then
            "good"
        else
            "really-bad"
    else
        "neutral"
