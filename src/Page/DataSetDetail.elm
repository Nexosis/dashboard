module Page.DataSetDetail exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes
import Data.Cascade as Cascade
import Data.Config exposing (Config)
import Data.Context exposing (ContextModel)
import Data.DataSet as DataSet exposing (ColumnStats, ColumnStatsDict, DataSet, DataSetData, DataSetName, DataSetStats, dataSetNameToString, toDataSetName)
import Data.DisplayDate exposing (toShortDateString)
import Data.Link exposing (Link, linkDecoder)
import Data.Session exposing (SessionData, SessionList)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (encodeUri)
import RemoteData as Remote
import Request.DataSet
import Request.Log as Log exposing (logHttpError)
import Request.Session exposing (getForDataset)
import Util exposing ((=>), commaFormatInteger, dataSizeWithSuffix, formatDisplayName, styledNumber)
import View.Breadcrumb as Breadcrumb
import View.ColumnMetadataEditor as ColumnMetadataEditor
import View.CopyableText exposing (copyableText)
import View.DeleteDialog as DeleteDialog
import View.Error as Error


---- MODEL ----


type alias Model =
    { dataSetName : DataSetName
    , loadingResponse : Remote.WebData DataSetData
    , columnMetadataEditorModel : ColumnMetadataEditor.Model
    , deleteDialogModel : Maybe DeleteDialog.Model
    , sessionLinks : SessionLinks
    , updateResponse : Remote.WebData ()
    }


type alias SessionLinks =
    { links : List Link
    }


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    let
        loadData =
            Request.DataSet.getRetrieveDetail config dataSetName
                |> Remote.sendRequest
                |> Cmd.map DataSetDataResponse

        ( editorModel, initCmd ) =
            ColumnMetadataEditor.init dataSetName True
    in
    Model dataSetName Remote.Loading editorModel Nothing (SessionLinks []) Remote.NotAsked
        ! [ loadData
          , loadRelatedSessions config dataSetName
          , Cmd.map ColumnMetadataEditorMsg initCmd
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
    | MetadataUpdated (Remote.WebData ())


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        DataSetDataResponse resp ->
            let
                ( subModel, cmd ) =
                    ColumnMetadataEditor.updateDataSetResponse context model.columnMetadataEditorModel resp
            in
            { model | loadingResponse = resp, columnMetadataEditorModel = subModel }
                => Cmd.map ColumnMetadataEditorMsg cmd

        ColumnMetadataEditorMsg subMsg ->
            let
                ( ( newModel, cmd ), updateMsg ) =
                    ColumnMetadataEditor.update subMsg model.columnMetadataEditorModel context

                requestMsg =
                    case updateMsg of
                        ColumnMetadataEditor.NoOp ->
                            Cmd.none

                        ColumnMetadataEditor.Updated modifiedMetadata ->
                            Request.DataSet.updateMetadata context.config (Request.DataSet.MetadataUpdateRequest model.dataSetName modifiedMetadata)
                                |> Remote.sendRequest
                                |> Cmd.map MetadataUpdated
            in
            { model | columnMetadataEditorModel = newModel }
                => Cmd.batch [ requestMsg, Cmd.map ColumnMetadataEditorMsg cmd ]

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
            case listResp of
                Remote.Success sessionList ->
                    let
                        subList =
                            List.map (\s -> s.links) sessionList.items
                                |> List.concat
                                |> List.filter (\l -> l.rel == "self")
                    in
                    { model | sessionLinks = SessionLinks subList } => Cmd.none

                Remote.Failure err ->
                    model => logHttpError err

                _ ->
                    model => Cmd.none

        MetadataUpdated response ->
            let
                metadataModel =
                    model.columnMetadataEditorModel

                newMetadataModel =
                    { metadataModel
                        | modifiedMetadata = Dict.empty
                        , columnMetadata =
                            metadataModel.columnMetadata
                                |> Remote.map (\cm -> { cm | metadata = Dict.union metadataModel.modifiedMetadata cm.metadata })
                    }
            in
            case response of
                Remote.Success () ->
                    { model | columnMetadataEditorModel = newMetadataModel, updateResponse = response }
                        => Cmd.none

                Remote.Failure err ->
                    { model | updateResponse = response }
                        => logHttpError err

                _ ->
                    model => Cmd.none


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
        , viewDetailsRow model
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ viewError model
                , ColumnMetadataEditor.view context model.columnMetadataEditorModel |> Html.map ColumnMetadataEditorMsg
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
            [ h2 [ ] [ text (formatDisplayName <| DataSet.dataSetNameToString model.dataSetName) ] ]
        , div [ class "col-sm-6 right" ]
            [ a [ AppRoutes.href (AppRoutes.SessionStart model.dataSetName), class "btn btn-danger mt10" ] [ text "Start Session" ]
            ]
        ]


viewError : Model -> Html Msg
viewError model =
    case model.updateResponse of
        Remote.Failure error ->
            div [ class "alert alert-danger" ] [ Error.viewHttpError error ]

        Remote.Success resp ->
            div [ class "alert alert-success" ] [ text "metadata updated" ]

        _ ->
            span [] []


viewDetailsRow : Model -> Html Msg
viewDetailsRow model =
    div [ id "details", class "row" ]
        [ viewRolesCol model
        , viewDetailsCol model
        , viewUrlAndDeleteCol model
        ]


viewRolesCol : Model -> Html Msg
viewRolesCol model =
    div [ class "col-sm-4" ]
        [ ColumnMetadataEditor.viewTargetAndKeyColumns model.columnMetadataEditorModel
            |> Html.map ColumnMetadataEditorMsg
        ]


viewUrlAndDeleteCol : Model -> Html Msg
viewUrlAndDeleteCol model =
    div [ class "col-sm-4" ]
        [ p []
            [ strong [] [ text "API Endpoint URL:" ]
            , br [] []
            , copyableText ("/data/" ++ (dataSetNameToString model.dataSetName |> encodeUri))
            ]
        , p []
            [ button [ class "btn btn-xs btn-primary", onClick ShowDeleteDialog ] [ i [ class "fa fa-trash-o mr5" ] [], text " Delete dataset" ]
            ]
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
        ]
