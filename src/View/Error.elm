module View.Error exposing (viewHttpError, viewMessagesAsError, viewRemoteError)

import Data.Message as Message exposing (Message)
import Data.Response exposing (ResponseError, responseErrorDecoder)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import RemoteData as Remote
import View.Extra exposing (viewIfElements, viewJust)


viewRemoteError : Remote.WebData response -> Html msg
viewRemoteError remoteResponse =
    case remoteResponse of
        Remote.Failure err ->
            div [ class "alert alert-danger" ]
                [ viewHttpError err ]

        _ ->
            div [] []


viewHttpError : Error -> Html msg
viewHttpError error =
    case error of
        Http.BadStatus badResponse ->
            responseErrorDecoder badResponse
                |> formatApiError
                |> List.singleton
                |> div []

        Http.Timeout ->
            div [] [ text "The request timed out.  Please check your connection and try again." ]

        Http.NetworkError ->
            div [] [ text "Network error encountered.  Please check your connection and try again." ]

        _ ->
            div [] [ text "An unexpected error occurred.  Please try again." ]


formatApiError : Data.Response.ResponseError -> Html msg
formatApiError error =
    let
        details =
            error.errorDetails |> Dict.toList
    in
    div []
        [ p []
            [ strong [] [ text "Status code: " ]
            , text <| toString error.statusCode
            ]
        , p []
            [ strong [] [ text "Message: " ]
            , text error.message
            ]
        , viewJust
            (\errorType ->
                p []
                    [ strong [] [ text "Type: " ]
                    , text errorType
                    ]
            )
            error.errorType
        , viewIfElements (\() -> ul [] (details |> List.map detailItem)) details
        ]


detailItem : ( String, String ) -> Html msg
detailItem ( key, value ) =
    li []
        [ strong [] [ text <| key ++ ": " ]
        , text value
        ]


viewMessagesAsError : List Message -> Html msg
viewMessagesAsError messages =
    let
        errorMessages =
            List.filter (\m -> m.severity == Message.Error) messages
    in
    viewIfElements
        (\() ->
            div [ class "alert alert-danger" ]
                (List.map
                    (\e -> p [] [ text e.message ])
                    errorMessages
                )
        )
        errorMessages
