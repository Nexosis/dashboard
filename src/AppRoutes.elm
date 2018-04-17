module AppRoutes exposing (Route(..), fromApiUrl, fromLocation, href, modifyUrl, newUrl, routeToString)

import Combine exposing ((<$>), (>>=))
import Html exposing (Attribute)
import Html.Attributes as Attr
import Http
import Navigation exposing (Location)
import Nexosis.Types.DataSet as DataSet exposing (DataSetName, toDataSetName)
import Route exposing ((</>), Route, Router, custom, int, match, route, router, static, string)
import String.Extra exposing (replace)
import Util exposing ((=>))


-- ROUTING --


type Route
    = Home
    | DataSets
    | DataSetDetail DataSet.DataSetName
    | DataSetAdd
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
        , route DataSetDetail (static "data" </> custom dataSetNameParser)
        , route DataSetAdd (static "addData")
        , route Sessions (static "sessions")
        , route SessionDetail (static "sessions" </> string)
        , route SessionStart (static "startSession" </> custom dataSetNameParser)
        , route Models (static "models")
        , route ModelDetail (static "models" </> string)
        ]


dataSetNameParser : Combine.Parser s DataSetName
dataSetNameParser =
    let
        encoded name =
            name
                |> Http.decodeUri
                |> Maybe.map Combine.succeed
                |> Maybe.withDefault (Combine.fail "can't unencode")
    in
    toDataSetName <$> (Combine.regex "[^/]+" >>= encoded)



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


fromLocation : Location -> Maybe ( Route, String )
fromLocation location =
    if String.isEmpty location.hash then
        Just ( Home, "Dashboard" )
    else
        match routeMatcher (String.dropLeft 1 location.hash)
            |> Maybe.map (\r -> r => lookupPageTitle r)


fromApiUrl : String -> String -> Maybe Route
fromApiUrl baseUrl apiUrl =
    let
        urlPart =
            replace baseUrl "" apiUrl
    in
    match routeMatcher urlPart


lookupPageTitle : Route -> String
lookupPageTitle route =
    case route of
        Home ->
            "Dashboard"

        DataSets ->
            "DataSets"

        DataSetDetail name ->
            "DataSet " ++ DataSet.dataSetNameToString name

        DataSetAdd ->
            "Add DataSet"

        Sessions ->
            "Sessions"

        SessionDetail _ ->
            "Session"

        SessionStart dataSetName ->
            "Start Session " ++ DataSet.dataSetNameToString dataSetName

        Models ->
            "Models"

        ModelDetail _ ->
            "Model Details"
