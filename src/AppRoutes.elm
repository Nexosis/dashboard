module AppRoutes exposing (Route(..), fromApiUrl, fromLocation, href, modifyUrl, newUrl, routeToString)

import Combine exposing ((<$>))
import Data.DataSet as DataSet
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import Route exposing ((</>), Route, Router, custom, int, match, route, router, static, string)
import String.Extra exposing (replace)


-- ROUTING --


type Route
    = Home
    | DataSets
    | DataSetDetail DataSet.DataSetName
    | DataSetAdd
    | Imports
    | Sessions
    | SessionDetail String
    | SessionStart DataSet.DataSetName
    | Models
    | ModelDetail String



{-
   These routes should match up with the actual API urls that will be called when interacting with the API.
   Not only is this a kind of nice thing for the user, we are going to use this mechanism for our own purposes.
   So, given a link to something in the API, "ml.nexosis.com/v2/data/datasetName", we can easily turn it into a url
   in the dashboard, "account.nexosis.com/dashboard/#/data/dataSetName".

-}


routeMatcher : Router Route
routeMatcher =
    router
        [ route Home (static "")
        , route DataSets (static "data")
        , route DataSetDetail (static "data" </> custom DataSet.dataSetNameParser)
        , route DataSetAdd (static "addData")
        , route Imports (static "imports")
        , route Sessions (static "sessions")
        , route SessionDetail (static "sessions" </> string)
        , route SessionStart (static "startSession" </> custom DataSet.dataSetNameParser)
        , route Models (static "models")
        , route ModelDetail (static "models" </> string)
        ]



-- PUBLIC HELPERS --


routeToString : Route -> String
routeToString page =
    let
        pagePath =
            case page of
                Home ->
                    []

                DataSets ->
                    [ "data" ]

                DataSetDetail name ->
                    [ "data", DataSet.dataSetNameToString name ]

                DataSetAdd ->
                    [ "addData" ]

                Imports ->
                    [ "imports" ]

                Sessions ->
                    [ "sessions" ]

                SessionDetail id ->
                    [ "sessions", id ]

                SessionStart dataSetName ->
                    [ "startSession", DataSet.dataSetNameToString dataSetName ]

                Models ->
                    [ "models" ]

                ModelDetail id ->
                    [ "models", id ]

        --    When needing parameters on the form base/item/3
        --                    Item id ->
        --                    [ "item",  id ]
    in
    "#/" ++ String.join "/" pagePath


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


newUrl : Route -> Cmd msg
newUrl =
    routeToString >> Navigation.newUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        match routeMatcher (String.dropLeft 1 location.hash)


fromApiUrl : String -> String -> Maybe Route
fromApiUrl baseUrl apiUrl =
    let
        urlPart =
            replace baseUrl "" apiUrl
    in
    match routeMatcher urlPart
