module DataBatchingTests exposing (..)

import Csv
import Data.Columns exposing (ColumnMetadata, defaultColumnMetadata)
import Data.File exposing (..)
import Dict
import Expect
import Test exposing (Test, describe, fuzz, fuzz2, test)


data : Int -> Int -> List (List ( String, String ))
data count width =
    let
        row val =
            List.map tup <| List.range 0 width

        tup val =
            ( "foo" ++ toString val, toString val )
    in
    List.map row <| List.range 0 count


file : Int -> Int -> JsonData
file count width =
    let
        row : List ( String, String ) -> Dict.Dict String String
        row l =
            Dict.fromList l
    in
    JsonData
        [ defaultColumnMetadata ]
        (List.map row (data count width))


batchingTests : Test
batchingTests =
    describe "jsonData batching tests"
        [ test "5 batches of 20 created for 100 rows"
            (\_ ->
                Expect.equal 5 <| List.length <| batchJsonData 20 (\a -> a) <| file 99 1
            )
        , test "6 batches created for 101 rows"
            (\_ ->
                Expect.equal 6 <| List.length <| batchJsonData 20 (\a -> a) <| file 100 1
            )
        , test "1 batches created for 2 rows"
            (\_ ->
                Expect.equal 1 <| List.length <| batchJsonData 20 (\a -> a) <| file 1 1
            )
        , test "columns sent with each batch"
            (\_ ->
                Expect.equal 6 <| List.length <| List.filter (not << List.isEmpty) <| batchJsonData 20 (\a -> a.columns) <| file 100 1
            )
        ]


batchSizeTests : Test
batchSizeTests =
    describe "batch size tests"
        [ test "calculates correct batch size for wide csv data"
            (\_ ->
                Expect.lessThan 1500 <| calculateCsvBatchSize <| Csv.parse wideCsvData
            )
        , test "calculates correct batch size for narrow csv data"
            (\_ ->
                Expect.greaterThan 30000 <| calculateCsvBatchSize <| Csv.parse narrowCsvData
            )
        , test "calculates correct batch size for narrow json data"
            (\_ ->
                Expect.greaterThan 5000 <| calculateJsonBatchSize <| narrowJsonData
            )
        , test "calculates correct batch size for wide json data"
            (\_ ->
                Expect.lessThan 100 <| calculateJsonBatchSize <| file 10 1000
            )
        ]


wideCsvData =
    """
    _unit_id,_golden,_unit_state,_trusted_judgments,_last_judgment_at,is_the_term,is_the_term:confidence,relevance,relevance:variance,definition,dist,example,freq,id,is_the_term_gold,len,relevance_gold,search,seed,term,topic
867851139,false,finalized,3,1/28/2016 19:02:18,a_specific_person_or_place,0.5019,4.0,0.0,"Any humanitarian crisis or natural disaster situation involving water supply, including lack of water, contamination of water, etc.",0.699343922434,"* There are also fears of disease from ruined sewage systems and drinking water sources, but Health Minister Mohammad Naseer Khan told Reuters vaccination teams were fanning out to innoculate people against diseases such as cholera and tetanus.",11.0,674,,3,,https://www.google.com/search?q=%22FROM%20RUINED%20SEWAGE%22,0,FROM RUINED SEWAGE,Water
    """


narrowCsvData =
    """
    iris,sepal_len,sepal_width,petal_len,petal_width
setosa,5.1,3.5,1.4,0.2
    """


narrowJsonData =
    JsonData [] [ Dict.fromList [ ( "sepal_len", "5.1" ), ( "sepal_width", "5.1" ), ( "petal_len", "5.1" ), ( "petal_width", "5.1" ), ( "iris", "setosa" ) ] ]
