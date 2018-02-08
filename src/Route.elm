module Route exposing (DataSetRoute(..), Route(..), fromLocation, href, modifyUrl)

import Data.DataSet as DataSet
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing (Parser, apply, follow, oneOf, parseLocation, parseUrl, path, return, string)


-- ROUTING --


type Route
    = Home
    | DataSetRoute DataSetRoute
    | Imports
    | Sessions
    | Models


type DataSetRoute
    = DataSets
    | DataSetDetail DataSet.DataSetName


routeMatcher : Parser Route
routeMatcher =
    oneOf
        [ return Home
        , return DataSetRoute |> follow (path "data") |> apply dataRoutes
        , return Imports |> follow (path "imports")
        , return Sessions |> follow (path "sessions")
        , return Models |> follow (path "models")
        ]


dataRoutes : Parser DataSetRoute
dataRoutes =
    oneOf
        [ return DataSets
        , return DataSetDetail |> apply DataSet.dataSetNameParser
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pagePath =
            case page of
                Home ->
                    []

                DataSetRoute dataSetRoute ->
                    case dataSetRoute of
                        DataSets ->
                            [ "data" ]

                        DataSetDetail name ->
                            [ "data", DataSet.dataSetNameToString name ]

                Imports ->
                    [ "imports" ]

                Sessions ->
                    [ "sessions" ]

                Models ->
                    [ "models" ]

        --    When needing parameters on the form base/item/3
        --                    Item id ->
        --                    [ "item",  id ]
    in
    "#/" ++ String.join "/" pagePath



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseUrl routeMatcher (String.dropLeft 1 location.hash)
