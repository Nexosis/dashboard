module Page.DataSets exposing (Model, Msg(DataSetListResponse), init, update, view)

import AppRoutes exposing (Route)
import Data.Cascade as Cascade
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import RemoteData as Remote
import Request.DataSet
import Request.Log as Log
import Set exposing (Set)
import Table exposing (defaultCustomizations)
import Util exposing ((=>), isJust)
import View.Error exposing (viewRemoteError)
import View.Grid as Grid
import View.Modal as Modal
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetList : Remote.WebData DataSetList
    , tableState : Table.State
    , config : Config
    , deleteDataSetPrompt : Maybe DataSetName
    , deleteConfirmInput : String
    , deleteConfirmEnabled : Bool
    , deleteCascadeOptions : Set String
    , deleteRequest : Remote.WebData ()
    }


loadDataSetList : Config -> Cmd Msg
loadDataSetList config =
    Request.DataSet.get config 0
        |> Remote.sendRequest
        |> Cmd.map DataSetListResponse


init : Config -> ( Model, Cmd Msg )
init config =
    Model "DataSets" "This is the list of DataSets" [] Remote.Loading (Table.initialSort "dataSetName") config Nothing "" False Set.empty Remote.NotAsked
        => loadDataSetList config



-- UPDATE --


type Msg
    = DataSetListResponse (Remote.WebData DataSetList)
    | SetTableState Table.State
    | ChangePage Int
    | ShowDeleteDialog DataSet
    | CancelDeleteDialog
    | DeleteTextBoxChanged String
    | CheckCascadeOption Cascade.Cascade Bool
    | DeleteDataSet
    | DeleteResponse (Remote.WebData ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        ChangePage pgNum ->
            { model | dataSetList = Remote.Loading }
                => (Request.DataSet.get model.config pgNum
                        |> Remote.sendRequest
                        |> Cmd.map DataSetListResponse
                   )

        ShowDeleteDialog dataSet ->
            { model | deleteDataSetPrompt = Just dataSet.dataSetName } => Cmd.none

        CancelDeleteDialog ->
            { model
                | deleteDataSetPrompt = Nothing
                , deleteCascadeOptions = Set.empty
                , deleteConfirmInput = ""
                , deleteConfirmEnabled = False
            }
                => Cmd.none

        DeleteTextBoxChanged content ->
            let
                isEnabled =
                    content == "DELETE"
            in
            { model
                | deleteConfirmInput = content
                , deleteConfirmEnabled = isEnabled
            }
                => Cmd.none

        CheckCascadeOption cascade checked ->
            let
                cascadeOptions =
                    if checked then
                        Set.insert (toString cascade) model.deleteCascadeOptions
                    else
                        Set.remove (toString cascade) model.deleteCascadeOptions
            in
            { model | deleteCascadeOptions = cascadeOptions } => Cmd.none

        DeleteDataSet ->
            case model.deleteDataSetPrompt of
                Just dataSetName ->
                    model
                        => (Request.DataSet.delete model.config dataSetName model.deleteCascadeOptions
                                |> Remote.sendRequest
                                |> Cmd.map DeleteResponse
                           )

                Nothing ->
                    model => Cmd.none

        DeleteResponse response ->
            case response of
                Remote.Success () ->
                    { model
                        | deleteDataSetPrompt = Nothing
                        , deleteCascadeOptions = Set.empty
                        , deleteConfirmInput = ""
                        , deleteConfirmEnabled = False
                        , deleteRequest = Remote.NotAsked
                    }
                        => loadDataSetList model.config

                Remote.Failure err ->
                    { model | deleteRequest = response }
                        => Log.logHttpError err

                _ ->
                    { model | deleteRequest = response }
                        => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        --todo - breadcrumbs ?
        [ p [ class "breadcrumb" ] [ span [] [ a [ href "#" ] [ text "API Dashboard" ] ] ]
        , div [ class "row" ]
            [ div [ class "col-sm-6" ]
                [ h2 [ class "mt10" ] [ text "Datasets" ]
                ]
            , div [ class "col-sm-6 right" ]
                [ -- todo - link somewhere
                  a [ href "#", class "btn mt10" ] [ i [ class "fa fa-plus mr5" ] [], text "Add DataSet" ]
                ]
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "row mb25" ]
                    [ div [ class "col-sm-6" ]
                        [ h3 [] [ text "Dataset explainer" ]
                        ]
                    , div [ class "col-sm-2 col-sm-offset-4 right" ]
                        [ div [ class "mr5" ]
                            -- change items per page
                            [ label [] [ text "View" ]
                            , select []
                                [ option [] [ text "10" ]
                                , option [] [ text "25" ]
                                ]
                            ]
                        ]
                    ]
                , Grid.view .items (config model.config.toolTips) model.tableState model.dataSetList
                , hr [] []
                , div [ class "center" ]
                    [ Pager.view model.dataSetList ChangePage ]
                ]
            ]
        , Modal.view
            (case model.deleteDataSetPrompt of
                Just toDeleteDataSet ->
                    Just
                        { closeMessage = CancelDeleteDialog
                        , header = Just deleteModalHeader
                        , body = Just (deleteModalBody toDeleteDataSet model.deleteRequest)
                        , footer = Just (deleteModalFooter model.deleteConfirmEnabled model.deleteRequest)
                        }

                Nothing ->
                    Nothing
            )
        ]


config : Dict String String -> Grid.Config DataSet Msg
config toolTips =
    Grid.config
        { toId = \a -> a.dataSetName |> dataSetNameToString
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , actionsColumn
            , Grid.customStringColumn "Size" sizeToString [ class "per10" ] []
            , Grid.customUnsortableColumn "Shape" (\_ -> "100 x 50") [ class "per15" ] (helpIcon toolTips "Shape")
            , Grid.customStringColumn "Created" (\_ -> "12/26/16") [ class "per10" ] []
            , Grid.customStringColumn "Modified" (\_ -> "12/25/17") [ class "per10" ] []
            , deleteColumn
            ]
        }


nameColumn : Grid.Column DataSet Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Name"
        , viewData = dataSetNameCell
        , sorter = Table.increasingOrDecreasingBy (\a -> a.dataSetName |> dataSetNameToString)
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


dataSetNameCell : DataSet -> Table.HtmlDetails Msg
dataSetNameCell dataSet =
    Table.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.DataSetDetail dataSet.dataSetName) ] [ text (dataSetNameToString dataSet.dataSetName) ] ]


actionsColumn : Grid.Column DataSet Msg
actionsColumn =
    Grid.veryCustomColumn
        { name = ""
        , viewData = dataSetActionButton
        , sorter = Table.unsortable
        , headAttributes = []
        , headHtml = []
        }


dataSetActionButton : DataSet -> Table.HtmlDetails Msg
dataSetActionButton dataSet =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        --todo - Change the button text and color based on the status
        [ button [ class "btn btn-sm" ] [ text "Start Session" ] ]


deleteColumn : Grid.Column DataSet Msg
deleteColumn =
    Grid.veryCustomColumn
        { name = "Delete"
        , viewData = dataSetDeleteButton
        , sorter = Table.unsortable
        , headAttributes = [ class "per5" ]
        , headHtml = []
        }


dataSetDeleteButton : DataSet -> Table.HtmlDetails Msg
dataSetDeleteButton dataSet =
    Table.HtmlDetails []
        [ button [ onClick (ShowDeleteDialog dataSet), alt "Delete", class "btn-link" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]


sizeToString : DataSet -> String
sizeToString dataSet =
    if dataSet.dataSetSize == 0 then
        "-"
    else
        toString dataSet.dataSetSize ++ "B"


deleteModalHeader : Html Msg
deleteModalHeader =
    h4 [ class "modal-title", style [ ( "color", "#fff" ), ( "font-weight", "700" ) ] ] [ text "Delete DataSet" ]


deleteModalBody : DataSetName -> Remote.WebData () -> Html Msg
deleteModalBody dataSetName deleteRequest =
    div []
        [ h5 []
            [ text "Are you sure you want to delete "
            , strong [] [ text (dataSetNameToString dataSetName) ]
            , text "?"
            ]
        , p [] [ text "This action cannot be undone but you can always upload it again in the future." ]
        , p [] [ text "Type ", strong [] [ text "\"DELETE\"" ], text "and then press \"confirm\" to delete." ]
        , div [ class "row m10" ]
            [ div [ class "col-sm-4" ]
                [ div [ class "form-group" ]
                    [ input [ class "form-control", placeholder "DELETE", onInput DeleteTextBoxChanged ] []
                    ]
                ]
            ]
        , div [ class "form-group" ]
            [ p [ class "small" ] [ text "Do you want to delete associated assets?" ]
            , div [ class "checkbox ml25" ]
                [ label [] [ input [ type_ "checkbox", onCheck (CheckCascadeOption Cascade.View) ] [], text "Views" ] ]
            , div [ class "checkbox ml25" ]
                [ label [] [ input [ type_ "checkbox", onCheck (CheckCascadeOption Cascade.Session) ] [], text "Sessions" ] ]
            , div [ class "checkbox ml25" ]
                [ label [] [ input [ type_ "checkbox", onCheck (CheckCascadeOption Cascade.Model) ] [], text "Models" ] ]
            , div [ class "checkbox ml25" ]
                [ label [] [ input [ type_ "checkbox", onCheck (CheckCascadeOption Cascade.Vocabulary) ] [], text "Vocabulary" ] ]
            ]
        , viewRemoteError deleteRequest
        ]


deleteModalFooter : Bool -> Remote.WebData () -> Html Msg
deleteModalFooter confirmEnabled deleteRequest =
    let
        deleteButton =
            case deleteRequest of
                Remote.Loading ->
                    button [ class "btn btn-primary", disabled True, onClick DeleteDataSet ] [ i [ class "fa fa-spinner fa-spin fa-2x fa-fw" ] [] ]

                _ ->
                    button [ class "btn btn-primary", disabled (not confirmEnabled), onClick DeleteDataSet ] [ text "Confirm" ]
    in
    div []
        [ button [ class "btn secondary", onClick CancelDeleteDialog ] [ text "Cancel" ]
        , deleteButton
        ]
