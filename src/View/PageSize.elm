module View.PageSize exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode exposing (Decoder, andThen, fail, field, string, succeed)


view : (Int -> msg) -> Html msg
view changeSize =
    div [ class "mr5" ]
        [ label [] [ text "View" ]
        , select [ on "change" (Json.Decode.map changeSize decodeInt) ]
            [ option [ value "10" ] [ text "10" ]
            , option [ value "25" ] [ text "25" ]
            , option [ value "50" ] [ text "50" ]
            , option [ value "100" ] [ text "100" ]
            ]
        ]


customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder d f =
    let
        resultDecoder x =
            case x of
                Ok a ->
                    succeed a

                Err e ->
                    fail e
    in
    Json.Decode.map f d |> andThen resultDecoder


decodeInt : Json.Decode.Decoder Int
decodeInt =
    customDecoder targetValue String.toInt
