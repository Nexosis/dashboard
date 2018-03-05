module Util exposing ((=>), commaFormatInteger, dataSizeWithSuffix, formatFloatToString, isJust, spinner)

import Html
import Html.Attributes


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>


isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        Just _ ->
            True


spinner : Html.Html msg
spinner =
    Html.i [ Html.Attributes.class "fa fa-spinner fa-spin fa-2x fa-fw" ] []


dataSizeWithSuffix : Int -> String
dataSizeWithSuffix size =
    if size == 0 then
        " - "
    else
        let
            k =
                size // 1024
        in
        case k of
            0 ->
                commaFormatInteger size ++ " B"

            _ ->
                if k >= 10000 then
                    commaFormatInteger (k // 1024) ++ " Mb"
                else
                    commaFormatInteger k ++ " Kb"


commaFormatInteger : Int -> String
commaFormatInteger value =
    String.join "," (splitThousands (toString value))


splitThousands : String -> List String
splitThousands integers =
    let
        reversedSplitThousands : String -> List String
        reversedSplitThousands value =
            if String.length value > 3 then
                value
                    |> String.dropRight 3
                    |> reversedSplitThousands
                    |> (::) (String.right 3 value)
            else
                [ value ]
    in
    integers
        |> reversedSplitThousands
        |> List.reverse


formatFloatToString : Float -> String
formatFloatToString input =
    let
        expand =
            toString (ceiling (input * 100000))

        len =
            String.length expand

        filled =
            String.padLeft 5 '0' expand
    in
    String.left (len - 5) filled ++ "." ++ String.right 5 filled
