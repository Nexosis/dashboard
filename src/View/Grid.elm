module View.Grid exposing (Column, Config, config, customStringColumn, customUnsortableColumn, floatColumn, intColumn, stringColumn, veryCustomColumn, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import List.Extra as ListX
import RemoteData as Remote
import Table exposing (..)


type alias Column data msg =
    { name : String
    , viewData : data -> HtmlDetails msg
    , sorter : Sorter data
    , headAttributes : List (Attribute msg)
    , headHtml : List (Html msg)
    }


type alias ColumnHeadConfig a msg =
    { a
        | headAttributes : List (Attribute msg)
        , headHtml : List (Html msg)
    }


type Config data msg
    = Config
        { toId : data -> String
        , toMsg : Table.State -> msg
        , columns : List (Column data msg)
        , customizations : Customizations data msg
        }


config :
    { toId : data -> String
    , toMsg : Table.State -> msg
    , columns : List (Column data msg)
    }
    -> Config data msg
config config =
    Config
        { toId = config.toId
        , toMsg = config.toMsg
        , columns = config.columns
        , customizations = defaultCustomizations
        }


customConfig :
    { toId : data -> String
    , toMsg : Table.State -> msg
    , columns : List (Column data msg)
    , customizations : Customizations data msg
    }
    -> Config data msg
customConfig config =
    Config config


mapConfig : Config data msg -> Table.Config data msg
mapConfig (Config { toId, toMsg, columns, customizations }) =
    let
        customs =
            addColumnCustomizations customizations columns

        tableConfig =
            { toId = toId
            , toMsg = toMsg
            , columns = List.map mapColumns columns
            , customizations = customs
            }
    in
    Table.customConfig tableConfig


addColumnCustomizations : Customizations data msg -> List (ColumnHeadConfig a msg) -> Customizations data msg
addColumnCustomizations customizations columnCustomizations =
    if customizations.thead == defaultCustomizations.thead then
        { customizations | thead = toTableHeadAttrs columnCustomizations, tableAttrs = toTableAttrs }
    else
        customizations


toTableAttrs : List (Attribute msg)
toTableAttrs =
    [ id "dataset-details", class "table table-striped" ]


mapColumns : Column data msg -> Table.Column data msg
mapColumns { name, viewData, sorter } =
    Table.veryCustomColumn { name = name, viewData = viewData, sorter = sorter }


customStringColumn : String -> (data -> String) -> List (Attribute msg) -> List (Html msg) -> Column data msg
customStringColumn name toStr attributes html =
    { name = name
    , viewData = textDetails << toStr
    , sorter = Table.increasingOrDecreasingBy toStr
    , headAttributes = attributes
    , headHtml = html
    }


customUnsortableColumn : String -> (data -> String) -> List (Attribute msg) -> List (Html msg) -> Column data msg
customUnsortableColumn name toStr attributes html =
    { name = name
    , viewData = textDetails << toStr
    , sorter = Table.unsortable
    , headAttributes = attributes
    , headHtml = html
    }


stringColumn : String -> (data -> String) -> Column data msg
stringColumn name toStr =
    { name = name
    , viewData = textDetails << toStr
    , sorter = Table.increasingOrDecreasingBy toStr
    , headAttributes = []
    , headHtml = []
    }


intColumn : String -> (data -> Int) -> Column data msg
intColumn name toInt =
    { name = name
    , viewData = textDetails << toString << toInt
    , sorter = Table.increasingOrDecreasingBy toInt
    , headAttributes = []
    , headHtml = []
    }


floatColumn : String -> (data -> String) -> Column data msg
floatColumn name toFloat =
    { name = name
    , viewData = textDetails << toString << toFloat
    , sorter = Table.increasingOrDecreasingBy toFloat
    , headAttributes = []
    , headHtml = []
    }


textDetails : String -> HtmlDetails msg
textDetails str =
    HtmlDetails [] [ Html.text str ]


veryCustomColumn :
    { name : String
    , viewData : data -> HtmlDetails msg
    , sorter : Sorter data
    , headAttributes : List (Attribute msg)
    , headHtml : List (Html msg)
    }
    -> Column data msg
veryCustomColumn column =
    { name = column.name
    , viewData = column.viewData
    , sorter = column.sorter
    , headAttributes = column.headAttributes
    , headHtml = column.headHtml
    }


toTableHeadAttrs : List (ColumnHeadConfig a msg) -> List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
toTableHeadAttrs headerConfig headers =
    let
        thList =
            headers
                |> ListX.zip headerConfig
                |> List.map headerCell
    in
    Table.HtmlDetails [] thList


headerCell : ( ColumnHeadConfig a msg, ( String, Table.Status, Attribute msg ) ) -> Html msg
headerCell ( headerConfig, ( name, status, onClick ) ) =
    let
        content =
            case status of
                Table.Unsortable ->
                    [ Html.text name ] ++ headerConfig.headHtml

                Table.Sortable selected ->
                    [ Html.text name ]
                        ++ headerConfig.headHtml
                        ++ [ if selected then
                                darkGray "sort-down"
                             else
                                mediumGray "sort-down"
                           ]

                Table.Reversible Nothing ->
                    [ Html.text name ]
                        ++ headerConfig.headHtml
                        ++ [ mediumGray "sort" ]

                Table.Reversible (Just isReversed) ->
                    [ Html.text name ]
                        ++ headerConfig.headHtml
                        ++ [ darkGray
                                (if isReversed then
                                    "sort-up"
                                 else
                                    "sort-down"
                                )
                           ]
    in
    Html.th ([ onClick ] ++ headerConfig.headAttributes) content


mediumGray : String -> Html msg
mediumGray icon =
    i [ class ("fa fa-" ++ icon ++ " color-mediumGray m15") ] []


darkGray : String -> Html msg
darkGray icon =
    i [ class ("fa fa-" ++ icon ++ " color-darkGray m15") ] []


view : (response -> List data) -> Config data msg -> Table.State -> Remote.WebData response -> Html.Html msg
view toData config state response =
    let
        tableConfig =
            mapConfig config

        (Config { toId, toMsg, columns, customizations }) =
            config
    in
    case response of
        Remote.Success successResponse ->
            let
                items =
                    successResponse |> toData
            in
            div [ class "table-responsive" ]
                [ Table.view tableConfig state items
                ]

        Remote.Failure err ->
            let
                fakeHeaders =
                    columns
                        |> List.map (\col -> th col.headAttributes [])

                columnCount =
                    List.length columns

                fakeRows =
                    List.repeat 9 (tr [] [ td [ colspan columnCount ] [ div [ class "line-height" ] [] ] ])

                errorMessageRow =
                    tr [] [ td [ colspan columnCount ] [ text (niceErrorMessage err) ] ]
            in
            div [ class "table-responsive" ]
                [ Table.view tableConfig state []
                , table [ id "dataset-details", class "table table-striped" ]
                    [ thead []
                        [ tr []
                            fakeHeaders
                        ]
                    , tbody []
                        (errorMessageRow
                            :: fakeRows
                        )
                    ]
                ]

        -- NotAsked and Loading
        _ ->
            let
                loadingHeaders =
                    columns
                        |> List.map (\col -> th col.headAttributes [])

                columnCount =
                    List.length columns

                loadingTds =
                    List.repeat columnCount (td [] [ div [ class "loading--line" ] [] ])

                loadingRows =
                    List.repeat 10 (tr [] loadingTds)
            in
            div [ class "table-responsive" ]
                [ Table.view tableConfig state []
                , table [ id "dataset-details", class "table table-striped" ]
                    [ thead []
                        [ tr []
                            loadingHeaders
                        ]
                    , tbody []
                        loadingRows
                    ]
                ]


niceErrorMessage : Http.Error -> String
niceErrorMessage error =
    case error of
        Http.Timeout ->
            "The request timed out.  Please check your connection and try again."

        Http.NetworkError ->
            "Network error encountered.  Please check your connection and try again."

        _ ->
            "An unexpected error occurred.  Please try again."
            