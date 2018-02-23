module View.DeleteDialog exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Data.Cascade as Cascade
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import RemoteData as Remote
import Request.Log as Log
import Set
import Util exposing ((=>), spinner)
import View.Error exposing (viewRemoteError)
import View.Extra exposing (viewJust)
import View.Modal as Modal


type alias Model =
    { displayName : String
    , id : String
    , selectedCascadeOptions : Set.Set String
    , confirmInput : String
    , confirmEnabled : Bool
    , deleteResponse : Remote.WebData ()
    }


type Msg
    = ShowDeleteDialog String
    | ConfirmDelete
    | CancelDeleteDialog
    | DeleteTextBoxChanged String
    | CheckCascadeOption Cascade.Cascade Bool
    | DeleteResponse (Remote.WebData ())


type ExternalMsg
    = NoOp
    | Confirmed


init : String -> String -> Model
init displayName id =
    { displayName = displayName
    , id = id
    , selectedCascadeOptions = Set.empty
    , confirmInput = ""
    , confirmEnabled = False
    , deleteResponse = Remote.NotAsked
    }


update : Maybe Model -> Msg -> (String -> Set.Set String -> Http.Request ()) -> ( ( Maybe Model, Cmd Msg ), ExternalMsg )
update maybeModel msg pendingDeleteCmd =
    case maybeModel of
        Just model ->
            case msg of
                ShowDeleteDialog dataSet ->
                    Just model => Cmd.none => NoOp

                CancelDeleteDialog ->
                    Nothing => Cmd.none => NoOp

                DeleteTextBoxChanged content ->
                    let
                        isEnabled =
                            content == "DELETE"
                    in
                    Just
                        { model
                            | confirmInput = content
                            , confirmEnabled = isEnabled
                        }
                        => Cmd.none
                        => NoOp

                CheckCascadeOption cascade checked ->
                    let
                        cascadeOptions =
                            if checked then
                                Set.insert (toString cascade) model.selectedCascadeOptions
                            else
                                Set.remove (toString cascade) model.selectedCascadeOptions
                    in
                    Just { model | selectedCascadeOptions = cascadeOptions } => Cmd.none => NoOp

                ConfirmDelete ->
                    let
                        deleteCmd =
                            pendingDeleteCmd model.id model.selectedCascadeOptions
                                |> Remote.sendRequest
                                |> Cmd.map DeleteResponse
                    in
                    Just { model | deleteResponse = Remote.Loading } => deleteCmd => NoOp

                DeleteResponse response ->
                    case response of
                        Remote.Success () ->
                            Nothing => Cmd.none => Confirmed

                        Remote.Failure err ->
                            Just { model | deleteResponse = response }
                                => Log.logHttpError err
                                => NoOp

                        _ ->
                            Just { model | deleteResponse = response } => Cmd.none => Confirmed

        Nothing ->
            Nothing => Cmd.none => NoOp


type alias DeleteConfig =
    { associatedAssets : List Cascade.Cascade
    , bodyMessage : Maybe String
    , headerMessage : String
    }


view : Maybe Model -> DeleteConfig -> Html Msg
view model deleteConfig =
    div []
        [ Modal.view
            (case model of
                Just toDeleteModel ->
                    Just
                        { closeMessage = CancelDeleteDialog
                        , header = Just (deleteModalHeader deleteConfig.headerMessage)
                        , body = Just (deleteModalBody toDeleteModel deleteConfig)
                        , footer = Just (deleteModalFooter toDeleteModel)
                        }

                Nothing ->
                    Nothing
            )
        ]


deleteModalHeader : String -> Html msg
deleteModalHeader headerMessage =
    h4 [ class "modal-title", style [ ( "color", "#fff" ), ( "font-weight", "700" ) ] ] [ text headerMessage ]


deleteModalBody : Model -> DeleteConfig -> Html Msg
deleteModalBody model deleteConfig =
    div []
        [ h5 []
            [ text "Are you sure you want to delete "
            , strong [] [ text model.displayName ]
            , text "?"
            ]
        , viewJust
            (\message ->
                p []
                    [ text message
                    ]
            )
            deleteConfig.bodyMessage
        , p [] [ text "Type ", strong [] [ text "\"DELETE\"" ], text "and then press \"confirm\" to delete." ]
        , div [ class "row m10" ]
            [ div [ class "col-sm-4" ]
                [ div [ class "form-group" ]
                    [ input [ class "form-control", placeholder "DELETE", onInput DeleteTextBoxChanged ] []
                    ]
                ]
            ]
        , div [ class "form-group" ]
            [ p [ class "small" ] <|
                text "Do you want to delete associated assets?"
                    :: List.map cascadeCheckbox deleteConfig.associatedAssets
            ]
        , viewRemoteError model.deleteResponse
        ]


cascadeCheckbox : Cascade.Cascade -> Html Msg
cascadeCheckbox cascadeOption =
    div [ class "checkbox ml25" ]
        [ label [] [ input [ type_ "checkbox", onCheck (CheckCascadeOption cascadeOption) ] [], text (toString cascadeOption) ] ]


deleteModalFooter : Model -> Html Msg
deleteModalFooter model =
    let
        deleteButton =
            case model.deleteResponse of
                Remote.Loading ->
                    button [ class "btn btn-primary", disabled True ] [ spinner ]

                _ ->
                    button [ class "btn btn-primary", disabled (not model.confirmEnabled), onClick ConfirmDelete ] [ text "Confirm" ]
    in
    div []
        [ button [ class "btn secondary", onClick CancelDeleteDialog ] [ text "Cancel" ]
        , deleteButton
        ]
