module Page.DataSetDetail exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes
import Data.Cascade as Cascade
import Data.Config exposing (Config)
import Data.Context exposing (ContextModel)
import Data.DataSet as DataSet exposing (ColumnStats, ColumnStatsDict, DataSet, DataSetData, DataSetName, DataSetStats, dataSetNameToString, toDataSetName)
import Data.DisplayDate exposing (toShortDateString)
import Data.Link exposing (Link, linkDecoder)
import Data.Session exposing (SessionData, SessionList)
import Data.Ziplist as Ziplist exposing (Ziplist)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (encodeUri)
import Page.DataSetData as DataSetData exposing (Model, Msg, init, update, view)
import RemoteData as Remote
import Request.DataSet
import Request.Session exposing (getForDataset)
import Util exposing ((=>), commaFormatInteger, dataSizeWithSuffix, formatDisplayName, styledNumber)
import View.Breadcrumb as Breadcrumb
import View.ColumnMetadataEditor as ColumnMetadataEditor
import View.CopyableText exposing (copyableText)
import View.DeleteDialog as DeleteDialog


---- MODEL ----


type alias Model =
    { dataSetName : DataSetName
    , loadingResponse : Remote.WebData DataSetData
    , columnMetadataEditorModel : ColumnMetadataEditor.Model
    , deleteDialogModel : Maybe DeleteDialog.Model
    , sessionResponse : Remote.WebData SessionList
    , tabs : Ziplist ( Tab, String )
    , dataSetDataModel : DataSetData.Model
    }


type alias SessionLinks =
    { links : List Link
    }


type Tab
    = MetadataTab
    | DataSetDataTab


init : ContextModel -> DataSetName -> ( Model, Cmd Msg )
init context dataSetName =
    let
        loadData =
            Request.DataSet.getRetrieveDetail context.config dataSetName 0 context.userPageSize
                |> Remote.sendRequest
                |> Cmd.map DataSetDataResponse

        ( editorModel, initCmd ) =
            ColumnMetadataEditor.init dataSetName True

        ( dataSetModel, _ ) =
            DataSetData.init context dataSetName
    in
    Model dataSetName Remote.Loading editorModel Nothing Remote.NotAsked initTabs dataSetModel
        ! [ loadData
          , loadRelatedSessions context.config dataSetName
          , Cmd.map ColumnMetadataEditorMsg initCmd
          ]


initTabs : Ziplist ( Tab, String )
initTabs =
    Ziplist.create []
        ( MetadataTab, "View metadata" )
        [ ( DataSetDataTab, "View data" )
        ]


loadRelatedSessions : Config -> DataSetName -> Cmd Msg
loadRelatedSessions config dataset =
    getForDataset config dataset
        |> Remote.sendRequest
        |> Cmd.map SessionDataListResponse



-- UPDATE --


type Msg
    = DataSetDataResponse (Remote.WebData DataSetData)
    | ShowDeleteDialog
    | DeleteDialogMsg DeleteDialog.Msg
    | SessionDataListResponse (Remote.WebData SessionList)
    | ColumnMetadataEditorMsg ColumnMetadataEditor.Msg
    | ChangeTab String
    | DataSetDataMsg DataSetData.Msg


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        DataSetDataResponse resp ->
            let
                ( subModel, cmd ) =
                    ColumnMetadataEditor.updateDataSetResponse context model.columnMetadataEditorModel resp

                ( dsModel, dsCmd ) =
                    DataSetData.dataUpdated context model.dataSetDataModel resp
            in
            { model | loadingResponse = resp, columnMetadataEditorModel = subModel, dataSetDataModel = dsModel }
                => Cmd.batch
                    [ Cmd.map ColumnMetadataEditorMsg cmd
                    , Cmd.map DataSetDataMsg dsCmd
                    ]

        ColumnMetadataEditorMsg subMsg ->
            let
                ( ( newModel, cmd ), updateMsg ) =
                    ColumnMetadataEditor.update subMsg
                        model.columnMetadataEditorModel
                        context
                        (\modifiedMetadata ->
                            Request.DataSet.updateMetadata context.config (Request.DataSet.MetadataUpdateRequest model.dataSetName modifiedMetadata)
                                |> Remote.sendRequest
                        )

                newMetadataModel =
                    case updateMsg of
                        ColumnMetadataEditor.NoOp ->
                            newModel

                        ColumnMetadataEditor.Updated modifiedMetadata ->
                            { newModel
                                | modifiedMetadata = Dict.empty
                                , columnMetadata =
                                    newModel.columnMetadata
                                        |> Remote.map (\cm -> { cm | metadata = Dict.union newModel.modifiedMetadata cm.metadata })
                            }
            in
            { model | columnMetadataEditorModel = newMetadataModel }
                => Cmd.map ColumnMetadataEditorMsg cmd

        ShowDeleteDialog ->
            let
                dsName =
                    dataSetNameToString model.dataSetName
            in
            { model | deleteDialogModel = Just (DeleteDialog.init dsName dsName) } => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                pendingDeleteCmd =
                    toDataSetName >> Request.DataSet.delete context.config

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            AppRoutes.modifyUrl AppRoutes.DataSets
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]

        SessionDataListResponse listResp ->
            { model | sessionResponse = listResp } => Cmd.none

        ChangeTab tabName ->
            let
                newTabs =
                    model.tabs
                        |> Ziplist.find (\( _, name ) -> name == tabName)
                        |> Maybe.withDefault model.tabs
            in
            { model | tabs = newTabs } => Cmd.none

        DataSetDataMsg subMsg ->
            let
                ( newModel, cmd ) =
                    DataSetData.update subMsg model.dataSetDataModel context
            in
            { model | dataSetDataModel = newModel }
                => Cmd.map DataSetDataMsg cmd


subscriptions : Model -> Sub Msg
subscriptions model =
    ColumnMetadataEditor.subscriptions model.columnMetadataEditorModel
        |> Sub.map ColumnMetadataEditorMsg



-- VIEW --


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ id "page-header", class "row" ]
            [ Breadcrumb.detail AppRoutes.DataSets "Datasets"
            , viewNameRow model
            ]
        , viewDetailsRow context model
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ viewTabControl model
                , viewTabContent context model
                ]
            ]
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete DataSet"
            , bodyMessage = Just "This action cannot be undone but you can always upload it again in the future."
            , associatedAssets = [ Cascade.View, Cascade.Session, Cascade.Model, Cascade.Vocabulary ]
            }
            |> Html.map DeleteDialogMsg
        ]


viewNameRow : Model -> Html Msg
viewNameRow model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ h2 [] [ text (formatDisplayName <| DataSet.dataSetNameToString model.dataSetName) ] ]
        , div [ class "col-sm-6 right" ]
            [ a [ AppRoutes.href (AppRoutes.SessionStart model.dataSetName), class "btn btn-danger mt10" ] [ text "Start Session" ]
            ]
        ]


viewDetailsRow : ContextModel -> Model -> Html Msg
viewDetailsRow context model =
    div [ id "details", class "row" ]
        [ viewDetailsCol model
        , viewRelatedCol model
        , viewRolesCol context model
        ]


viewRelatedCol : Model -> Html Msg
viewRelatedCol model =
    div [ class "col-sm-3" ]
        [ p [ attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#related", attribute "aria-expanded" "true", attribute "aria-controls" "related" ]
            [ strong [] [ text "Related Sessions" ]
            , i [ class "fa fa-angle-down ml5" ] []
            ]
        , ul [ id "related", class "collapse in", attribute "aria-expanded" "true" ]
            (getRelatedSessions model)
        ]


getRelatedSessions : Model -> List (Html Msg)
getRelatedSessions model =
    case model.sessionResponse of
        Remote.Success sessionList ->
            if List.isEmpty sessionList.items then
                [ li [] [ text "No related sessions..." ] ]
            else
                List.map sessionLinkItem sessionList.items

        _ ->
            [ li [] [ text "There was a problem accessing sessions..." ] ]


sessionLinkItem : SessionData -> Html Msg
sessionLinkItem session =
    let
        detailRef =
            AppRoutes.href (AppRoutes.SessionDetail session.sessionId)
    in
    li [] [ a [ detailRef ] [ text session.name ] ]


viewRolesCol : ContextModel -> Model -> Html Msg
viewRolesCol context model =
    div [ class "col-sm-5" ]
        [ ColumnMetadataEditor.viewTargetAndKeyColumns context model.columnMetadataEditorModel
            |> Html.map ColumnMetadataEditorMsg
        ]


viewDetailsCol : Model -> Html Msg
viewDetailsCol model =
    let
        ( size, shape, created, modified ) =
            case model.loadingResponse of
                Remote.Success resp ->
                    let
                        sizeDisplay =
                            styledNumber <| dataSizeWithSuffix resp.dataSetSize

                        shapeDisplay =
                            styledNumber <| commaFormatInteger resp.totalCount ++ " x " ++ toString (List.length resp.columns)

                        createdDisplay =
                            text <| toShortDateString resp.dateCreated

                        modifiedDisplay =
                            text <| toShortDateString resp.lastModified
                    in
                    ( sizeDisplay, shapeDisplay, createdDisplay, modifiedDisplay )

                Remote.Loading ->
                    let
                        loading =
                            -- this text node has magic \x00A0 characters instead of just spaces.
                            span [ class "loading--line" ] [ text "       " ]
                    in
                    ( loading, loading, loading, loading )

                _ ->
                    let
                        empty =
                            div [] []
                    in
                    ( empty, empty, empty, empty )
    in
    div [ class "col-sm-4" ]
        [ p []
            [ strong [] [ text "Size: " ]
            , size
            ]
        , p []
            [ strong [] [ text "Shape: " ]
            , shape
            ]
        , p []
            [ strong [] [ text "Created: " ]
            , created
            ]
        , p []
            [ strong [] [ text "Modified: " ]
            , modified
            ]
        , p []
            [ strong [] [ text "API Endpoint URL:" ]
            , br [] []
            , copyableText ("/data/" ++ (dataSetNameToString model.dataSetName |> encodeUri))
            ]
        , p []
            [ button [ class "btn btn-xs btn-primary", onClick ShowDeleteDialog ] [ i [ class "fa fa-trash-o mr5" ] [], text " Delete dataset" ]
            ]
        ]


viewTabControl : Model -> Html Msg
viewTabControl model =
    let
        tabHeaders =
            (model.tabs.previous |> List.map viewInactiveTab)
                ++ [ viewActiveTab model.tabs.current ]
                ++ (model.tabs.next |> List.map viewInactiveTab)
    in
    ul [ class "nav nav-tabs", attribute "role" "tablist" ]
        tabHeaders


viewInactiveTab : ( Tab, String ) -> Html Msg
viewInactiveTab ( _, tabText ) =
    li [] [ a [ attribute "role" "button", onClick (ChangeTab tabText) ] [ text tabText ] ]


viewActiveTab : ( Tab, String ) -> Html Msg
viewActiveTab ( _, tabText ) =
    li [ class "active" ] [ a [ attribute "role" "button", onClick (ChangeTab tabText) ] [ text tabText ] ]


viewTabContent : ContextModel -> Model -> Html Msg
viewTabContent context model =
    let
        content =
            case model.tabs.current of
                ( MetadataTab, _ ) ->
                    ColumnMetadataEditor.view context model.columnMetadataEditorModel |> Html.map ColumnMetadataEditorMsg

                ( DataSetDataTab, _ ) ->
                    DataSetData.view context model.dataSetDataModel |> Html.map DataSetDataMsg
    in
    div [ class "tab-content" ]
        [ div [ class "tab-pane active" ]
            [ content ]
        ]
