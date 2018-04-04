module View.Charts exposing (anomalyResults, distributionHistogram, forecastResults, impactResults, regressionResults, renderConfusionMatrix)

import Array
import Data.AggregationStrategy as AggregationStrategy
import Data.Columns as Columns exposing (ColumnMetadata)
import Data.ConfusionMatrix as ConfusionMatrix exposing (ConfusionMatrix)
import Data.DataSet exposing (DataSetData, DistributionShape)
import Data.DistanceMetric exposing (DistanceMetrics, DistanceValue, fromDistanceMetrics)
import Data.Session as Session exposing (SessionData, SessionResults)
import Dict exposing (Dict)
import Html exposing (Html, a, div, h3, node, span, table, tbody, td, tr)
import Html.Attributes exposing (attribute, class, colspan, href, rowspan, style, target)
import Json.Encode
import List.Extra as List exposing (find)
import String.Extra as String exposing (replace)
import VegaLite exposing (..)


renderChart : Spec -> Html msg
renderChart spec =
    node "vega-chart" [ attribute "spec" (Json.Encode.encode 0 spec) ] []


distributionHistogram : List DistributionShape -> Html msg
distributionHistogram data =
    let
        config =
            configure
                << configuration (Axis [ Labels False, Ticks False, Grid False, Domain False ])
                << configuration (Background "transparent")
                << configuration (View [ Stroke (Just "transparent") ])
                << configuration (MarkStyle [ MFill "#2bb7ec" ])

        enc =
            encoding
                << position X [ PName "Value", PmType Ordinal, PSort [] ]
                << position Y [ PName "Count", PmType Quantitative ]
    in
    toVegaLite
        [ width 150
        , height 60
        , padding (PEdges 0 0 0 0)
        , autosize [ ANone ]
        , dataFromRows [] <| List.concatMap distributionItemToRow data
        , mark Bar []
        , enc []
        , config []
        ]
        |> renderChart


distributionItemToRow : DistributionShape -> List DataRow
distributionItemToRow shape =
    let
        itemLabel =
            "Value"
    in
    case shape of
        Data.DataSet.Counts label count ->
            dataRow [ ( itemLabel, Str label ), ( "Count", Number (toFloat count) ) ] []

        Data.DataSet.Ranges min max count ->
            if min == max then
                dataRow [ ( itemLabel, Str min ), ( "Count", Number (toFloat count) ) ] []
            else
                dataRow [ ( itemLabel, Str (min ++ " to " ++ max) ), ( "Count", Number (toFloat count) ) ] []


forecastResults : SessionResults -> SessionData -> DataSetData -> Int -> Html msg
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
                        << position X [ PName (normalizeFieldName timestampCol.name), PmType Temporal, PTimeUnit (Utc YearMonthDateHoursMinutes), PAxis [ AxTitle "Timestamp", AxFormat (axisLabelFormat session) ] ]
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
            in
            toVegaLite
                [ VegaLite.title "Results"
                , VegaLite.width chartWidth
                , VegaLite.height chartHeight
                , autosize [ AFit, APadding ]
                , dataFromRows [] <| List.concatMap resultsToRows (sessionData ++ dataSetData)
                , VegaLite.mark Line [ MInterpolate Monotone ]
                , enc []
                ]
                |> renderChart

        _ ->
            span [] []


impactResults : SessionResults -> SessionData -> DataSetData -> Int -> Html msg
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
                        << position X [ PName (normalizeFieldName timestampCol.name), PmType Temporal, PTimeUnit (Utc YearMonthDateHoursMinutes), PAxis [ AxTitle "Timestamp", AxFormat (axisLabelFormat session) ] ]
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
                    position X [ PName (normalizeFieldName timestampCol.name), PmType Temporal, PTimeUnit (Utc YearMonthDateHoursMinutes), PAggregate Min ]

                maxPos =
                    position X [ PName (normalizeFieldName timestampCol.name), PmType Temporal, PTimeUnit (Utc YearMonthDateHoursMinutes), PAggregate Max ]

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
                        [ (encoding << minPos << position X2 [ PName timestampCol.name, PmType Temporal, PTimeUnit (Utc YearMonthDateHoursMinutes), PAggregate Max ]) []
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
                |> renderChart

        _ ->
            span [] []


axisLabelFormat : SessionData -> String
axisLabelFormat session =
    case session.resultInterval of
        Just Session.Hour ->
            "%x %X"

        _ ->
            "%x"


regressionResults : SessionResults -> SessionData -> Int -> Html msg
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

                -- Calculation of m and b have the axes reversed compared to convention (Wikipedia), but this is the correct line for our plot.
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
                |> renderChart

        _ ->
            span [] []


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


anomalyResults : SessionResults -> SessionData -> DistanceMetrics -> Int -> Html msg
anomalyResults sessionResults session metric windowWidth =
    let
        distanceValues =
            fromDistanceMetrics metric

        quartiles list =
            let
                findMedian list =
                    let
                        splitIndex =
                            List.length list // 2

                        ( bottom, top ) =
                            list |> List.splitAt splitIndex
                    in
                    ( bottom
                    , bottom |> List.drop (splitIndex - 1) |> List.getAt 0
                    , top
                    )

                ( lower, median, upper ) =
                    findMedian list

                ( _, q1, _ ) =
                    findMedian lower

                ( _, q3, _ ) =
                    findMedian upper
            in
            ( q1, median, q3 )

        filterDistance distances filter =
            distances |> List.filter filter |> List.map (\i -> i.distance ^ (1 / 3)) |> List.sort

        inliersQuartiles =
            quartiles (filterDistance distanceValues (\i -> i.anomaly >= 0))

        outliersQuartiles =
            quartiles (filterDistance distanceValues (\i -> i.anomaly < 0))

        metricToDataRow : DistanceValue -> List DataRow
        metricToDataRow item =
            if item.anomaly < 0 then
                dataRow [ ( "Category", Str "Outliers" ), ( "Value", Number (item.distance ^ (1 / 3)) ) ] []
            else
                dataRow [ ( "Category", Str "Inliers" ), ( "Value", Number (item.distance ^ (1 / 3)) ) ] []

        data =
            distanceValues |> List.map metricToDataRow

        transformAggregate =
            transform
                << aggregate [ opAs Q1 "Value" "Q1", opAs Median "Value" "Q2", opAs Q3 "Value" "Q3" ] [ "Category" ]
                << calculateAs "datum.Q3 - datum.Q1" "IQR"
                << calculateAs "datum.Q1 - datum.IQR * 1.5" "lowerWhisker"
                << calculateAs "datum.Q3 + datum.IQR * 1.5" "upperWhisker"

        whiskerSpec =
            asSpec
                [ (encoding
                    << position X [ PName "Category", PmType Nominal ]
                    << position Y [ PName "lowerWhisker", PmType Quantitative, PScale [ SZero False ] ]
                    << position Y2 [ PName "upperWhisker", PmType Quantitative ]
                  )
                    []
                , mark Rule [ MStyle [ "boxWhisker" ] ]
                , transformAggregate []
                ]

        boxSpec =
            asSpec
                [ (encoding
                    << position X [ PName "Category", PmType Nominal ]
                    << position Y [ PName "Q1", PmType Quantitative ]
                    << position Y2 [ PName "Q3", PmType Quantitative ]
                    << size [ MNumber 75 ]
                    << color [ MString "#2bb7ec" ]
                  )
                    []
                , mark Bar [ MStyle [ "box" ] ]
                , transformAggregate []
                ]

        tickSpec =
            asSpec
                [ (encoding
                    << position X [ PName "Category", PmType Nominal ]
                    << position Y [ PName "Q2", PmType Quantitative ]
                    << size [ MNumber 75 ]
                    << color [ MString "white" ]
                  )
                    []
                , mark Tick [ MStyle [ "boxMid" ] ]
                , transformAggregate []
                ]

        valuesProps =
            [ (encoding
                << position X [ PName "Category", PmType Nominal ]
                << position Y [ PName "Value", PmType Quantitative, PAxis [ AxTitle "Values" ] ]
                << color [ MString "#2bb7ec" ]
              )
                []
            , mark Circle [ MSize 150, MOpacity 0.25 ]
            ]

        valuesTransform quartiles categoryLabel =
            let
                ( maybeQ1, _, maybeQ3 ) =
                    quartiles

                q1 =
                    Maybe.withDefault 0 maybeQ1

                q3 =
                    Maybe.withDefault 0 maybeQ3

                iqr =
                    q3 - q1

                top =
                    q3 + iqr * 1.5

                bottom =
                    q1 - iqr * 1.5
            in
            transform
                << filter
                    (FExpr ("(datum.Category == '" ++ categoryLabel ++ "' && datum.Value > " ++ String.fromFloat top ++ ") || (datum.Category == '" ++ categoryLabel ++ "' && datum.Value < " ++ String.fromFloat bottom ++ ")"))

        inliersSpec =
            asSpec <| valuesTransform inliersQuartiles "Inliers" [] :: valuesProps

        outliersSpec =
            asSpec <| valuesTransform outliersQuartiles "Outliers" [] :: valuesProps

        ( chartWidth, chartHeight ) =
            widthToSize windowWidth
    in
    toVegaLite
        [ title "Results"
        , width chartWidth
        , height chartHeight
        , autosize [ AFit, APadding ]
        , dataFromRows [] <| List.concat data
        , layer
            [ whiskerSpec
            , boxSpec
            , tickSpec
            , inliersSpec
            , outliersSpec
            ]
        ]
        |> renderChart


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
    div []
        [ div [ class "row" ]
            [ div [ class "col-sm-6" ] [ h3 [] [ Html.text "Confusion Matrix" ] ]
            , div [ class "col-sm-6 text-right" ]
                [ a [ href "https://docs.nexosis.com/guides/analyzing-classification-results", target "_blank" ] [ div [ class "btn btn-default btn-sm" ] [ Html.text "Understanding your results" ] ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ table [ class "table table-bordered confusion-matrix" ]
                    [ tbody []
                        (List.map (\r -> toConfusionMatrixRow matrix.classes r) (Array.toIndexedList matrix.confusionMatrix)
                            -- footer is the set of classes
                            ++ [ tr [ class "footer" ] (td [] [] :: List.map (\c -> td [] [ div [] [ span [] [ Html.text c ] ] ]) (Array.toList matrix.classes)) ]
                        )
                    ]
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
