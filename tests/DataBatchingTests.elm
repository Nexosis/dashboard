module DataBatchingTests exposing (..)

import Data.Columns exposing (ColumnMetadata, defaultColumnMetadata)
import Data.File exposing (..)
import Dict
import Expect
import Test exposing (Test, describe, fuzz, fuzz2, test)


data : Int -> List (List ( String, String ))
data count =
    let
        tup val =
            [ ( "foo", toString val ), ( "bar", toString val ) ]
    in
    List.map tup <| List.range 0 count


file : Int -> JsonData
file count =
    let
        row : List ( String, String ) -> Dict.Dict String String
        row l =
            Dict.fromList l
    in
    JsonData
        [ defaultColumnMetadata ]
        (List.map row (data count))


batchingTests : Test
batchingTests =
    describe "jsonData batching tests"
        [ test "5 batches of 20 created for 100 rows"
            (\_ ->
                Expect.equal 5 <| List.length <| batchJsonData 20 (\a -> a) <| file 99
            )
        , test "6 batches created for 101 rows"
            (\_ ->
                Expect.equal 6 <| List.length <| batchJsonData 20 (\a -> a) <| file 100
            )
        , test "1 batches created for 2 rows"
            (\_ ->
                Expect.equal 1 <| List.length <| batchJsonData 20 (\a -> a) <| file 1
            )
        , test "columns sent with each batch"
            (\_ ->
                Expect.equal 6 <| List.length <| List.filter (not << List.isEmpty) <| batchJsonData 20 (\a -> a.columns) <| file 100
            )
        ]
