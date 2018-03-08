module Util exposing ((=>), commaFormatInteger, dataSizeWithSuffix, formatFloatToString, isActuallyInteger, isJust, spinner, styledNumber)

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
    if not <| isActuallyInteger input then
        let
            expand =
                toString (ceiling (input * 100000))

            len =
                String.length expand

            filled =
                String.padLeft 5 '0' expand

            result =
                trimRightZeroes (String.left (len - 5) filled ++ "." ++ String.right 5 filled)
        in
        if String.left 1 result == "." then
            "0" ++ result
        else
            result
    else
        commaFormatInteger <| truncate input


trimRightZeroes : String -> String
trimRightZeroes input =
    let
        strings =
            String.split "." input

        left =
            Maybe.withDefault "" (List.head strings)

        right =
            Maybe.withDefault [] (List.tail strings)
    in
    if right == [ "" ] then
        left
    else
        case String.reverse input |> String.uncons of
            Just ( h, tl ) ->
                if h == '0' then
                    trimRightZeroes <| String.reverse tl
                else
                    input

            Nothing ->
                ""


isActuallyInteger : Float -> Bool
isActuallyInteger input =
    (input / 1.0 - (toFloat <| round input)) == 0


styledNumber : String -> Html.Html msg
styledNumber input =
    Html.span [ Html.Attributes.class "number" ] [ Html.text input ]
