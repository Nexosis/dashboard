module AppRoutes exposing (Route(..), fromApiUrl, fromLocation, href, modifyUrl)

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
    | Imports
    | Sessions
    | SessionDetail String
    | Models



{-
   These routes should match up with the actual API urls that will be called when interacting with the API.
   Not only is this a kind of nice thing for the user, we are going to use this mechanism for our own purposes.
   So, given a link to something in the API, "ml.nexosis.com/v2/data/datasetName", we can easily turn it into a url
   in the dashboard, "account.nexosis.com/dashboard/#/data/dataSetName".

-}
--routeMatcher : Parser Route


routeMatcher : Router Route
routeMatcher =
    router
        [ route Home (static "")
        , route DataSets (static "data")
        , route DataSetDetail (static "data" </> custom DataSet.dataSetNameParser)
        , route Imports (static "imports")
        , route Sessions (static "sessions")
        , route SessionDetail (static "sessions" </> string)
        , route Models (static "models")
        ]



-- INTERNAL --


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

                Imports ->
                    [ "imports" ]

                Sessions ->
                    [ "sessions" ]

                SessionDetail id ->
                    [ "sessions", id ]

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
        match routeMatcher (String.dropLeft 1 location.hash)


fromApiUrl : String -> String -> Maybe Route
fromApiUrl baseUrl apiUrl =
    let
        urlPart =
            replace baseUrl "" apiUrl
    in
    match routeMatcher urlPart
