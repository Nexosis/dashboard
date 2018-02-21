module View.DeleteDialog exposing(Model, Msg, deleteModalHeader, deleteModalBody, deleteModalFooter)

import Html.Attributes exposing (..)
import Html.Events exposing(onClick, onCheck, onInput)
import Html exposing (..)
import Data.Cascade as Cascade
import RemoteData as Remote
import View.Error exposing (viewRemoteError)


type alias Model = {
    displayName : String
   ,id : String
   ,message : Maybe String
   ,associatedAssets : List Cascade.Cascade
}

type Msg = 
  ConfirmDelete
  | CancelDeleteDialog
  | DeleteTextBoxChanged String
  | CheckCascadeOption Cascade.Cascade Bool

deleteModalHeader : Html msg
deleteModalHeader =
    h4 [ class "modal-title", style [ ( "color", "#fff" ), ( "font-weight", "700" ) ] ] [ text "Delete DataSet" ]

deleteModalBody : Model -> Remote.WebData () -> Html Msg
deleteModalBody model deleteRequest =
    div []
        [ h5 []
            [ text "Are you sure you want to delete "
            , strong [] [ text (model.displayName) ]
            , text "?"
            ]
        , p [] [ text (case model.message of 
                          Nothing -> "This action cannot be undone but you can always upload it again in the future." 
                          Just val -> val)  ]
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


deleteModalFooter : msg -> msg -> Bool -> Remote.WebData () -> Html msg
deleteModalFooter confirmDeleteMessage cancelMessage confirmEnabled deleteRequest =
    let
        deleteButton =
            case deleteRequest of
                Remote.Loading ->
                    button [ class "btn btn-primary", disabled True, onClick confirmDeleteMessage ] [ i [ class "fa fa-spinner fa-spin fa-2x fa-fw" ] [] ]

                _ ->
                    button [ class "btn btn-primary", disabled (not confirmEnabled), onClick confirmDeleteMessage ] [ text "Confirm" ]
    in
    div []
        [ button [ class "btn secondary", onClick cancelMessage ] [ text "Cancel" ]
        , deleteButton
        ]