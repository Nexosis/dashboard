module View.Charts exposing (forecastResults, regressionResults, renderConfusionMatrix)

import Array
import Data.Columns as Columns exposing (ColumnMetadata)
import Data.ConfusionMatrix as ConfusionMatrix exposing (ConfusionMatrix)
import Data.Session as Session exposing (SessionData, SessionResults)
import Dict exposing (Dict)
import Html exposing (Html, div, h3, table, tbody, td, tr)
import Html.Attributes exposing (class, style)
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
                [ VegaLite.title "Results"
                , VegaLite.width chartWidth
                , VegaLite.height chartHeight
                , autosize [ AFit, APadding ]
                , dataFromRows [] <| List.concatMap resultsToRows sessionResults.data
                , VegaLite.mark Line []
                , enc []
                ]

        _ ->
            toVegaLite
                []


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

                sumXSquare =
                    List.foldl (\( x, _ ) s -> s + x ^ 2) 0 dataValues

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

                meanXSquare =
                    sumXSquare / valuesLength

                meanSquareX =
                    meanX ^ 2

                m =
                    (meanXY - (meanX * meanY)) / (meanXSquare - meanSquareX)

                b =
                    meanY - m * meanX

                actualName =
                    targetCol.name ++ ":actual"

                pointTypeName =
                    "Result Type"

                enc =
                    encoding
                        << position Y [ PName targetCol.name, PmType Quantitative ]
                        << position X [ PName actualName, PmType Quantitative ]
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
                    [ lineSpec
                    , pointSpec
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
                    td [ style [ ( "backgroundColor", colorFromValue rowMax value (index == rowNumber) ) ] ] [ Html.text (toString value) ]
                )
                (Array.toIndexedList data)
        )


colorFromValue : Int -> Int -> Bool -> String
colorFromValue maxValue value isTarget =
    let
        scaled =
            round ((toFloat value / toFloat maxValue) * 100)
    in
    if scaled == 0 then
        if isTarget then
            "#F23131"
        else
            "#CABDBD"
    else if scaled < 33 then
        "#EFE975"
    else if scaled < 66 then
        "#FFB01D"
    else if scaled <= 100 then
        if isTarget then
            "#2DB27D"
        else
            "#F23131"
    else
        "#4CFFFC"
