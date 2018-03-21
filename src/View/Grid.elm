module View.Grid
    exposing
        ( Column
        , Config
        , HtmlDetails
        , ReadOnlyTableMsg(..)
        , State
        , config
        , configCustom
        , customNumberColumn
        , customStringColumn
        , customUnsortableColumn
        , decreasingOrIncreasingBy
        , floatColumn
        , increasingOrDecreasingBy
        , initialSort
        , intColumn
        , makeUnsortable
        , stringColumn
        , toFixedTable
        , unsortable
        , veryCustomColumn
        , view
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2, lazy3)
import Http
import Json.Decode as Json
import List.Extra as ListX
import RemoteData as Remote


type ReadOnlyTableMsg
    = Readonly State


type State
    = State String Bool


initialSort : String -> State
initialSort header =
    State header False


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
        }


customConfig :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    , customizations : Customizations data msg
    }
    -> Config data msg
customConfig { toId, toMsg, columns, customizations } =
    Config
        { toId = toId
        , toMsg = toMsg
        , columns = List.map (\(Column cData) -> cData) columns
        , customizations = customizations
        }


type alias Customizations data msg =
    { tableAttrs : List (Attribute msg)
    , caption : Maybe (HtmlDetails msg)
    , thead : List ( ColumnHeadConfig msg, String, Status, Attribute msg ) -> HtmlDetails msg
    , tfoot : Maybe (HtmlDetails msg)
    , tbodyAttrs : List (Attribute msg)
    , rowAttrs : data -> List (Attribute msg)
    }



-- mapConfig : Config data msg -> Config data msg
-- mapConfig (Config { toId, toMsg, columns, customizations }) =
--     let
--         customs =
--             addColumnCustomizations customizations columns
--         tableConfig =
--             { toId = toId
--             , toMsg = toMsg
--             , columns = List.map mapColumns columns
--             , customizations = customs
--             }
--     in
--     customConfig tableConfig
-- addColumnCustomizations : Customizations data msg -> List (ColumnHeadConfig a msg) -> Customizations data msg
-- addColumnCustomizations customizations columnCustomizations =
--     let
--         headCust cust =
--             if cust.thead == defaultCustomizations.thead then
--                 { cust | thead = toTableHeadAttrs columnCustomizations }
--             else
--                 cust
--         tableAttributes cust =
--             if cust.tableAttrs == defaultCustomizations.tableAttrs then
--                 { cust | tableAttrs = toTableAttrs }
--             else
--                 cust
--     in
--     customizations |> headCust |> tableAttributes


toFixedTable : Customizations data msg -> Customizations data msg
toFixedTable defaultCustomizations =
    { defaultCustomizations | tableAttrs = [ id "dataset-details", class "table table-striped fixed" ] } |> Debug.log "attrs"


toTableAttrs : List (Attribute msg)
toTableAttrs =
    [ id "dataset-details", class "table table-striped" ]



-- mapColumns : Column data msg -> Column data msg
-- mapColumns { name, viewData, sorter } =
--     veryCustomColumn { name = name, viewData = viewData, sorter = sorter }


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



-- toTableHeadAttrs : List (ColumnHeadConfig a msg) -> List ( String, Status, Attribute msg ) -> HtmlDetails msg
-- toTableHeadAttrs headerConfig headers =
--     let
--         thList =
--             headers
--                 |> ListX.zip headerConfig
--                 |> List.map headerCell
--     in
--     HtmlDetails [] thList


headerCell : ( ColumnHeadConfig msg, String, Status, Attribute msg ) -> Html msg
headerCell ( headerConfig, name, status, onClick ) =
    let
        content =
            case status of
                Unsortable ->
                    [ Html.text name ] ++ headerConfig.headHtml

                Sortable selected ->
                    [ Html.text name ]
                        ++ headerConfig.headHtml
                        ++ [ if selected then
                                darkGray "sort-down"
                             else
                                mediumGray "sort-down"
                           ]

                Reversible Nothing ->
                    [ Html.text name ]
                        ++ headerConfig.headHtml
                        ++ [ mediumGray "sort" ]

                Reversible (Just isReversed) ->
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


view : (response -> List data) -> Config data msg -> State -> Remote.WebData response -> Html.Html msg
view toData config state response =
    let
        -- tableConfig =
        --     mapConfig config
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
                [ viewOld config state items ]

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
                [ viewOld config state []
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
                [ viewOld config state []
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
    | Sortable Bool
    | Reversible (Maybe Bool)



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


viewOld : Config data msg -> State -> List data -> Html msg
viewOld (Config { toId, toMsg, columns, customizations }) state data =
    let
        sortedData =
            sort state columns data

        theadDetails =
            customizations.thead (List.map (toHeaderInfo state toMsg) columns)

        thead =
            Html.thead theadDetails.attributes theadDetails.children

        tbody =
            Keyed.node "tbody" customizations.tbodyAttrs <|
                List.map (viewRow toId columns customizations.rowAttrs) sortedData

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
toHeaderInfo (State sortName isReversed) toMsg { name, sorter, headAttributes, headHtml } =
    let
        columnHeadConfig =
            { headAttributes = headAttributes, headHtml = headHtml }
    in
    case sorter of
        None ->
            ( columnHeadConfig, name, Unsortable, onClick sortName isReversed toMsg )

        Increasing _ ->
            ( columnHeadConfig, name, Sortable (name == sortName), onClick name False toMsg )

        Decreasing _ ->
            ( columnHeadConfig, name, Sortable (name == sortName), onClick name False toMsg )

        IncOrDec _ ->
            if name == sortName then
                ( columnHeadConfig, name, Reversible (Just isReversed), onClick name (not isReversed) toMsg )
            else
                ( columnHeadConfig, name, Reversible Nothing, onClick name False toMsg )

        DecOrInc _ ->
            if name == sortName then
                ( columnHeadConfig, name, Reversible (Just isReversed), onClick name (not isReversed) toMsg )
            else
                ( columnHeadConfig, name, Reversible Nothing, onClick name False toMsg )


onClick : String -> Bool -> (State -> msg) -> Attribute msg
onClick name isReversed toMsg =
    E.on "click" <|
        Json.map toMsg <|
            Json.map2 State (Json.succeed name) (Json.succeed isReversed)


viewRow : (data -> String) -> List (ColumnData data msg) -> (data -> List (Attribute msg)) -> data -> ( String, Html msg )
viewRow toId columns toRowAttrs data =
    ( toId data
    , lazy3 viewRowHelp columns toRowAttrs data
    )


viewRowHelp : List (ColumnData data msg) -> (data -> List (Attribute msg)) -> data -> Html msg
viewRowHelp columns toRowAttrs data =
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
sort (State selectedColumn isReversed) columnData data =
    case findSorter selectedColumn columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter isReversed sorter data


applySorter : Bool -> Sorter data -> List data -> List data
applySorter isReversed sorter data =
    case sorter of
        None ->
            data

        Increasing sort ->
            sort data

        Decreasing sort ->
            List.reverse (sort data)

        IncOrDec sort ->
            if isReversed then
                List.reverse (sort data)
            else
                sort data

        DecOrInc sort ->
            if isReversed then
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
    | Increasing (List data -> List data)
    | Decreasing (List data -> List data)
    | IncOrDec (List data -> List data)
    | DecOrInc (List data -> List data)


{-| A sorter for columns that are unsortable. Maybe you have a column in your
table for delete buttons that delete the row. It would not make any sense to
sort based on that column.
-}
unsortable : Sorter data
unsortable =
    None


{-| Create a sorter that can only display the data in increasing order. If we
want a table of people, sorted alphabetically by name, we would say this:

    sorter : Sorter { a | name : comparable }
    sorter =
      increasingBy .name

-}
increasingBy : (data -> comparable) -> Sorter data
increasingBy toComparable =
    Increasing (List.sortBy toComparable)


{-| Create a sorter that can only display the data in decreasing order. If we
want a table of countries, sorted by population from highest to lowest, we
would say this:

    sorter : Sorter { a | population : comparable }
    sorter =
      decreasingBy .population

-}
decreasingBy : (data -> comparable) -> Sorter data
decreasingBy toComparable =
    Decreasing (List.sortBy toComparable)


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
