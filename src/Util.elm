module Util exposing ((=>), isJust, spinner)

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
