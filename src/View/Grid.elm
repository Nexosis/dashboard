module View.Grid
    exposing
        ( Column
        , Config
        , HtmlDetails
        , ReadOnlyTableMsg(..)
        , Sorter
        , State
        , config
        , configCustom
        , customNumberColumn
        , customStringColumn
        , customUnsortableColumn
        , decreasingOrIncreasingBy
        , defaultCustomizations
        , floatColumn
        , increasingOrDecreasingBy
        , initialSort
        , initialUnsorted
        , intColumn
        , makeUnsortable
        , remoteConfig
        , remoteConfigCustom
        , stringColumn
        , unsortable
        , veryCustomColumn
        , view
        )

-- This module source was copied from https://github.com/evancz/elm-sortable-table and modified to not sort if specified,
-- as well as using different defaults.

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2, lazy3)
import Http
import Json.Decode as Json
import NexosisHelpers exposing (SortDirection(..), SortParameters)
import RemoteData as Remote


type ReadOnlyTableMsg
    = Readonly State


type alias State =
    SortParameters


initialSort : String -> SortDirection -> State
initialSort header direction =
    { sortName = header, direction = direction }


initialUnsorted : SortParameters
initialUnsorted =
    { sortName = "", direction = Ascending }


{-| Describes how to turn `data` into a column in your table.
-}
type Column data msg
    = Column (ColumnData data msg)


type alias ColumnData data msg =
    { name : String
    , viewData : data -> HtmlDetails msg
    , sorter : Sorter data
    , headAttributes : List (Attribute msg)
    , headHtml : List (Html msg)
    }


type alias ColumnHeadConfig msg =
    { headAttributes : List (Attribute msg)
    , headHtml : List (Html msg)
    }


type Config data msg
    = Config
        { toId : data -> String
        , toMsg : State -> msg
        , columns : List (ColumnData data msg)
        , customizations : Customizations data msg
        , isRemote : Bool
        }


configCustom :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    , customizations : Customizations data msg -> Customizations data msg
    }
    -> Config data msg
configCustom config =
    Config
        { toId = config.toId
        , toMsg = config.toMsg
        , columns = List.map (\(Column cData) -> cData) config.columns
        , customizations = config.customizations defaultCustomizations
        , isRemote = False
        }


remoteConfigCustom :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    , customizations : Customizations data msg -> Customizations data msg
    }
    -> Config data msg
remoteConfigCustom config =
    Config
        { toId = config.toId
        , toMsg = config.toMsg
        , columns = List.map (\(Column cData) -> cData) config.columns
        , customizations = config.customizations defaultCustomizations
        , isRemote = True
        }


config :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    }
    -> Config data msg
config config =
    Config
        { toId = config.toId
        , toMsg = config.toMsg
        , columns = List.map (\(Column cData) -> cData) config.columns
        , customizations = defaultCustomizations
        , isRemote = False
        }


remoteConfig :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    }
    -> Config data msg
remoteConfig config =
    Config
        { toId = config.toId
        , toMsg = config.toMsg
        , columns = List.map (\(Column cData) -> cData) config.columns
        , customizations = defaultCustomizations
        , isRemote = True
        }


type alias Customizations data msg =
    { tableAttrs : List (Attribute msg)
    , caption : Maybe (HtmlDetails msg)
    , thead : List ( ColumnHeadConfig msg, String, Status, Attribute msg ) -> HtmlDetails msg
    , tfoot : Maybe (HtmlDetails msg)
    , tbodyAttrs : List (Attribute msg)
    , rowAttrs : data -> List (Attribute msg)
    , rowOverride : RowOverrideFunctions data msg
    }


type alias RowOverrideFunctions data msg =
    ( data -> Bool, data -> Html msg )


toTableAttrs : List (Attribute msg)
toTableAttrs =
    [ id "dataset-details", class "table table-striped" ]


customStringColumn : String -> (data -> String) -> List (Attribute msg) -> List (Html msg) -> Column data msg
customStringColumn name toStr attributes html =
    Column
        { name = name
        , viewData = textDetails << toStr
        , sorter = increasingOrDecreasingBy toStr
        , headAttributes = attributes
        , headHtml = html
        }


customNumberColumn : String -> (data -> String) -> List (Attribute msg) -> List (Html msg) -> Column data msg
customNumberColumn name toStr attributes html =
    Column
        { name = name
        , viewData = numberDetails << toStr
        , sorter = increasingOrDecreasingBy toStr
        , headAttributes = attributes
        , headHtml = html
        }


customUnsortableColumn : String -> (data -> String) -> List (Attribute msg) -> List (Html msg) -> Column data msg
customUnsortableColumn name toStr attributes html =
    Column
        { name = name
        , viewData = textDetails << toStr
        , sorter = unsortable
        , headAttributes = attributes
        , headHtml = html
        }


stringColumn : String -> (data -> String) -> Column data msg
stringColumn name toStr =
    Column
        { name = name
        , viewData = textDetails << toStr
        , sorter = increasingOrDecreasingBy toStr
        , headAttributes = []
        , headHtml = []
        }


intColumn : String -> (data -> Int) -> Column data msg
intColumn name toInt =
    Column
        { name = name
        , viewData = textDetails << toString << toInt
        , sorter = increasingOrDecreasingBy toInt
        , headAttributes = []
        , headHtml = []
        }


floatColumn : String -> (data -> String) -> Column data msg
floatColumn name toFloat =
    Column
        { name = name
        , viewData = textDetails << toString << toFloat
        , sorter = increasingOrDecreasingBy toFloat
        , headAttributes = []
        , headHtml = []
        }


textDetails : String -> HtmlDetails msg
textDetails str =
    HtmlDetails [] [ Html.text str ]


numberDetails : String -> HtmlDetails msg
numberDetails str =
    HtmlDetails [ class "number" ] [ Html.text str ]


veryCustomColumn :
    { name : String
    , viewData : data -> HtmlDetails msg
    , sorter : Sorter data
    , headAttributes : List (Attribute msg)
    , headHtml : List (Html msg)
    }
    -> Column data msg
veryCustomColumn column =
    Column
        { name = column.name
        , viewData = column.viewData
        , sorter = column.sorter
        , headAttributes = column.headAttributes
        , headHtml = column.headHtml
        }


makeUnsortable : Column data msg -> Column data msg
makeUnsortable (Column column) =
    Column
        { column
            | sorter = unsortable
        }


headerCell : ( ColumnHeadConfig msg, String, Status, Attribute msg ) -> Html msg
headerCell ( headerConfig, name, status, onClick ) =
    let
        heading =
            if headerConfig.headHtml /= [] then
                headerConfig.headHtml
            else
                [ Html.text name ]

        content =
            case status of
                Unsortable ->
                    heading

                Reversible Nothing ->
                    heading ++ [ mediumGray "sort" ]

                Reversible (Just isReversed) ->
                    heading
                        ++ [ darkGray
                                (if isReversed == Ascending then
                                    "sort-up"
                                 else
                                    "sort-down"
                                )
                           ]
    in
    Html.th ([ onClick ] ++ headerConfig.headAttributes) content


mediumGray : String -> Html msg
mediumGray icon =
    i [ class ("fa fa-" ++ icon ++ " color-mediumGray ml5") ] []


darkGray : String -> Html msg
darkGray icon =
    i [ class ("fa fa-" ++ icon ++ " color-darkGray ml5") ] []


view : (response -> List data) -> Config data msg -> State -> Remote.WebData response -> Html.Html msg
view toData config state response =
    let
        (Config { toId, toMsg, columns, customizations }) =
            config
    in
    case response of
        Remote.Success successResponse ->
            let
                items =
                    successResponse |> toData
            in
            div []
                [ viewTable config state items ]

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
            div []
                [ viewTable config state []
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
            div []
                [ viewTable config state []
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


{-| Sometimes you must use a `<td>` tag, but the attributes and children are up
to you. This type lets you specify all the details of an HTML node except the
tag name.
-}
type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| The customizations used in `config` by default.
-}
defaultCustomizations : Customizations data msg
defaultCustomizations =
    { tableAttrs = toTableAttrs
    , caption = Nothing
    , thead = simpleThead
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = simpleRowAttrs
    , rowOverride = ( \d -> False, \d -> div [] [] )
    }


simpleThead : List ( ColumnHeadConfig msg, String, Status, Attribute msg ) -> HtmlDetails msg
simpleThead headers =
    HtmlDetails [] (List.map headerCell headers)


simpleRowAttrs : data -> List (Attribute msg)
simpleRowAttrs _ =
    []


{-| The status of a particular column, for use in the `thead` field of your
`Customizations`.

  - If the column is unsortable, the status will always be `Unsortable`.
  - If the column can be sorted in one direction, the status will be `Sortable`.
    The associated boolean represents whether this column is selected. So it is
    `True` if the table is currently sorted by this column, and `False` otherwise.
  - If the column can be sorted in either direction, the status will be `Reversible`.
    The associated maybe tells you whether this column is selected. It is
    `Just isReversed` if the table is currently sorted by this column, and
    `Nothing` otherwise. The `isReversed` boolean lets you know which way it
    is sorted.

This information lets you do custom header decorations for each scenario.

-}
type Status
    = Unsortable
    | Reversible (Maybe SortDirection)



-- COLUMNS


{-| Perhaps the basic columns are not quite what you want. Maybe you want to
display monetary values in thousands of dollars, and `floatColumn` does not
quite cut it. You could define a custom column like this:

    import Table

    dollarColumn : String -> (data -> Float) -> Column data msg
    dollarColumn name toDollars =
        Table.customColumn
            { name = name
            , viewData = \data -> viewDollars (toDollars data)
            , sorter = Table.decreasingBy toDollars
            }

    viewDollars : Float -> String
    viewDollars dollars =
        "$" ++ toString (round (dollars / 1000)) ++ "k"

The `viewData` field means we will displays the number `12345.67` as `$12k`.

The `sorter` field specifies how the column can be sorted. In `dollarColumn` we
are saying that it can _only_ be shown from highest-to-lowest monetary value.
More about sorters soon!

-}
customColumn :
    { name : String
    , viewData : data -> String
    , sorter : Sorter data
    }
    -> Column data msg
customColumn { name, viewData, sorter } =
    Column <|
        ColumnData name (textDetails << viewData) sorter [] []



-- VIEW


viewTable : Config data msg -> State -> List data -> Html msg
viewTable (Config { toId, toMsg, columns, customizations, isRemote }) state data =
    let
        sortedData =
            if isRemote then
                data
            else
                sort state columns data

        theadDetails =
            customizations.thead (List.map (toHeaderInfo state toMsg) columns)

        thead =
            Html.thead theadDetails.attributes theadDetails.children

        tbody =
            Keyed.node "tbody" customizations.tbodyAttrs <|
                List.map (viewRow toId columns customizations.rowAttrs customizations.rowOverride) sortedData

        withFoot =
            case customizations.tfoot of
                Nothing ->
                    tbody :: []

                Just { attributes, children } ->
                    Html.tfoot attributes children :: tbody :: []
    in
    Html.table customizations.tableAttrs <|
        case customizations.caption of
            Nothing ->
                thead :: withFoot

            Just { attributes, children } ->
                Html.caption attributes children :: thead :: withFoot


toHeaderInfo : State -> (State -> msg) -> ColumnData data msg -> ( ColumnHeadConfig msg, String, Status, Attribute msg )
toHeaderInfo { sortName, direction } toMsg { name, sorter, headAttributes, headHtml } =
    let
        columnHeadConfig =
            { headAttributes = headAttributes, headHtml = headHtml }
    in
    case sorter of
        None ->
            ( columnHeadConfig, name, Unsortable, onClick sortName direction toMsg )

        IncOrDec _ ->
            if name == sortName then
                ( columnHeadConfig, name, Reversible (Just direction), onClick name (flipDirection direction) toMsg )
            else
                ( columnHeadConfig, name, Reversible Nothing, onClick name Ascending toMsg )

        DecOrInc _ ->
            if name == sortName then
                ( columnHeadConfig, name, Reversible (Just direction), onClick name (flipDirection direction) toMsg )
            else
                ( columnHeadConfig, name, Reversible Nothing, onClick name Descending toMsg )


flipDirection : SortDirection -> SortDirection
flipDirection direction =
    if direction == Ascending then
        Descending
    else
        Ascending


onClick : String -> SortDirection -> (State -> msg) -> Attribute msg
onClick name direction toMsg =
    E.on "click" <|
        Json.map toMsg <|
            Json.map2 SortParameters (Json.succeed name) (Json.succeed direction)


viewRow : (data -> String) -> List (ColumnData data msg) -> (data -> List (Attribute msg)) -> RowOverrideFunctions data msg -> data -> ( String, Html msg )
viewRow toId columns toRowAttrs rowOverride data =
    ( toId data
    , lazy3 (viewRowHelp rowOverride) columns toRowAttrs data
    )


viewRowHelp : RowOverrideFunctions data msg -> List (ColumnData data msg) -> (data -> List (Attribute msg)) -> data -> Html msg
viewRowHelp ( rowTest, rowRender ) columns toRowAttrs data =
    if rowTest data then
        rowRender data
    else
        Html.tr (toRowAttrs data) (List.map (viewCell data) columns)


viewCell : data -> ColumnData data msg -> Html msg
viewCell data { viewData } =
    let
        details =
            viewData data
    in
    Html.td details.attributes details.children



-- SORTING


sort : State -> List (ColumnData data msg) -> List data -> List data
sort { sortName, direction } columnData data =
    case findSorter sortName columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter direction sorter data


applySorter : SortDirection -> Sorter data -> List data -> List data
applySorter direction sorter data =
    case sorter of
        None ->
            data

        IncOrDec sort ->
            if direction == Ascending then
                List.reverse (sort data)
            else
                sort data

        DecOrInc sort ->
            if direction == Descending then
                sort data
            else
                List.reverse (sort data)


findSorter : String -> List (ColumnData data msg) -> Maybe (Sorter data)
findSorter selectedColumn columnData =
    case columnData of
        [] ->
            Nothing

        { name, sorter } :: remainingColumnData ->
            if name == selectedColumn then
                Just sorter
            else
                findSorter selectedColumn remainingColumnData



-- SORTERS


{-| Specifies a particular way of sorting data.
-}
type Sorter data
    = None
    | IncOrDec (List data -> List data)
    | DecOrInc (List data -> List data)


{-| A sorter for columns that are unsortable. Maybe you have a column in your
table for delete buttons that delete the row. It would not make any sense to
sort based on that column.
-}
unsortable : Sorter data
unsortable =
    None


{-| Sometimes you want to be able to sort data in increasing _or_ decreasing
order. Maybe you have a bunch of data about orange juice, and you want to know
both which has the most sugar, and which has the least sugar. Both interesting!
This function lets you see both, starting with decreasing order.

    sorter : Sorter { a | sugar : comparable }
    sorter =
        decreasingOrIncreasingBy .sugar

-}
decreasingOrIncreasingBy : (data -> comparable) -> Sorter data
decreasingOrIncreasingBy toComparable =
    DecOrInc (List.sortBy toComparable)


{-| Sometimes you want to be able to sort data in increasing _or_ decreasing
order. Maybe you have race times for the 100 meter sprint. This function lets
sort by best time by default, but also see the other order.

    sorter : Sorter { a | time : comparable }
    sorter =
        increasingOrDecreasingBy .time

-}
increasingOrDecreasingBy : (data -> comparable) -> Sorter data
increasingOrDecreasingBy toComparable =
    IncOrDec (List.sortBy toComparable)
