module FileBatchingTests exposing (..)

import Data.Columns exposing (ColumnMetadata, defaultColumnMetadata)
import Data.File exposing (..)
import Dict
import Expect
import Fuzz exposing (..)
import List.Extra exposing (last)
import Test exposing (Test, describe, fuzz, fuzz2, test)


data : Int -> List (List ( String, String ))
data count =
    let
        tup val =
            [ ( "foo", toString val ), ( "bar", toString val ) ]
    in
    List.map tup <| List.range 0 count


file : Int -> JsonFile
file count =
    let
        row : List ( String, String ) -> Dict.Dict String String
        row l =
            Dict.fromList l
    in
    JsonFile
        [ defaultColumnMetadata ]
        (List.map row (data count))


fuzzTests : Test
fuzzTests =
    describe "jsonFile batching tests"
        [ test "5 batches of 20 created for 100 rows"
            (\_ ->
                Expect.equal 5 <| List.length <| batchJsonFile 20 (\a -> a) <| file 99
            )
        , test "6 batches created for 101 rows"
            (\_ ->
                Expect.equal 6 <| List.length <| batchJsonFile 20 (\a -> a) <| file 100
            )
        , test "1 batches created for 2 rows"
            (\_ ->
                Expect.equal 1 <| List.length <| batchJsonFile 20 (\a -> a) <| file 1
            )
        , test "columns sent with each batch"
            (\_ ->
                Expect.equal 6 <| List.length <| List.filter (not << List.isEmpty) <| batchJsonFile 20 (\a -> a.columns) <| file 100
            )
        ]
