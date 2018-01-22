module Route exposing (Route(..), href, modifyUrl, fromLocation)

import UrlParser as Url exposing (parseHash, s, (</>), string, oneOf, Parser)
import Navigation exposing (Location)
import Html exposing (Attribute)
import Html.Attributes as Attr


-- ROUTING --


type Route
    = Home
    | DataSets
    | Imports
    | Sessions
    | Models



--    When needing parameters on the form base/item/id
--   | Item String


routeMatcher : Parser (Route -> a) a
routeMatcher =
    oneOf
        [ Url.map Home (s "")
        , Url.map DataSets (s "datasets")
        , Url.map Imports (s "imports")
        , Url.map Sessions (s "sessions")
        , Url.map Models (s "models")

        --    When needing parameters on the form base/item/3
        --    , Url.map Item (s "item" </> string)
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
                    [ "datasets" ]

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
        "#/" ++ (String.join "/" pagePath)



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
        parseHash routeMatcher location
