module View.Charts exposing (forecastResults, impactResults, renderConfusionMatrix)

import Array
import Data.Columns as Columns exposing (ColumnMetadata)
import Data.ConfusionMatrix as ConfusionMatrix exposing (ConfusionMatrix)
import Data.DataSet exposing (DataSetData)
import Data.Session as Session exposing (SessionData, SessionResults)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
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


impactResults : SessionData -> SessionResults -> DataSetData -> Int -> Spec
impactResults session sessionResults dataSet windowWidth =
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
                sessionData =
                    List.map (\dict -> Dict.insert "Source" "Predictions" dict) sessionResults.data

                dataSetData =
                    List.map (\dict -> Dict.insert "Source" "Observations" dict) dataSet.data

                enc =
                    encoding
                        << position X [ PName timestampCol.name, PmType Temporal, PTimeUnit <| resultIntervalToTimeUnit session.resultInterval ]
                        << position Y [ PName targetCol.name, PmType Quantitative ]
                        << color
                            [ MName "Source"
                            , MmType Nominal
                            , MScale <|
                                categoricalDomainMap
                                    [ ( "Predictions", "#1F77B4" )
                                    , ( "Observations", "#04850d" )
                                    ]
                            ]
            in
            toVegaLite
                [ VegaLite.title (Maybe.withDefault "" (Dict.get "event" session.extraParameters) ++ " Results")
                , VegaLite.width chartWidth
                , VegaLite.height chartHeight
                , autosize [ AFit, APadding ]
                , dataFromRows [] <| List.concatMap resultsToRows (sessionData ++ dataSetData)
                , VegaLite.mark Line [ MInterpolate Monotone ]
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
