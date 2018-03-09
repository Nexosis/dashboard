module View.PageSize exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode exposing (Decoder, andThen, fail, field, string, succeed)


view : (Int -> msg) -> Int -> Html msg
view changeSize pageSize =
    div [ class "mr5" ]
        [ label [] [ text "View" ]
        , select [ on "change" (Json.Decode.map changeSize decodeInt) ]
            [ option [ selected (pageSize == 10), value "10" ] [ text "10" ]
            , option [ selected (pageSize == 25), value "25" ] [ text "25" ]
            , option [ selected (pageSize == 50), value "50" ] [ text "50" ]
            , option [ selected (pageSize == 100), value "100" ] [ text "100" ]
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
