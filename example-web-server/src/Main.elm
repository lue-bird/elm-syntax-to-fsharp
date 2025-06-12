module Main exposing (view)

import Html
import Html.Attributes


view : String -> Html.Html Never
view name =
    Html.div []
        [ Html.h1 []
            [ Html.text ("Hello, " ++ name ++ "!") ]
        , Html.p []
            [ Html.text "This is pretty nice." ]
        , Html.a
            [ Html.Attributes.title "Evan Czaplicki, Public domain, via Wikimedia Commons"
            , Html.Attributes.href "https://commons.wikimedia.org/wiki/File:Elm_logo.svg"
            ]
            [ Html.img
                [ Html.Attributes.src "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Elm_logo.svg/256px-Elm_logo.svg.png?20160911065740"
                , Html.Attributes.alt "the elm logo"
                , Html.Attributes.width 256
                ]
                [ Html.text "the elm logo" ]
            ]
        ]
