module Main exposing (Event, State, initialState, update, view)

import Html
import Html.Attributes
import Html.Events
import Json.Decode


type alias State =
    { count : Int
    , mouseX : Int
    , mouseY : Int
    }


type Event
    = CounterClicked
    | MouseMoved { x : Int, y : Int }


initialState : State
initialState =
    { count = 0
    , mouseX = 0
    , mouseY = 0
    }


view : State -> Html.Html Event
view state =
    Html.div
        [ Html.Events.on "mousemove"
            (Json.Decode.map2 (\x y -> MouseMoved { x = x, y = y })
                (Json.Decode.field "clientX" Json.Decode.int)
                (Json.Decode.field "clientY" Json.Decode.int)
            )
        ]
        [ Html.h1 []
            [ Html.text "Hello, wanderer!" ]
        , Html.p []
            [ Html.text
                ("You're at x="
                    ++ (state.mouseX |> String.fromInt)
                    ++ " y="
                    ++ (state.mouseY |> String.fromInt)
                )
            ]
        , Html.button
            [ Html.Events.onClick CounterClicked
            ]
            [ Html.text (String.fromInt state.count)
            ]
        , Html.br [] []
        , Html.br [] []
        , Html.a
            [ Html.Attributes.title "Evan Czaplicki, Public domain, via Wikimedia Commons"
            , Html.Attributes.href "https://commons.wikimedia.org/wiki/File:Elm_logo.svg"
            ]
            [ Html.img
                [ Html.Attributes.src "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Elm_logo.svg/256px-Elm_logo.svg.png?20160911065740"
                , Html.Attributes.alt "the elm logo"
                , Html.Attributes.width 100
                ]
                [ Html.text "the elm logo" ]
            ]
        ]


update : Event -> State -> State
update event state =
    case event of
        CounterClicked ->
            { state | count = state.count + 1 }

        MouseMoved newMousePosition ->
            { state
                | mouseX = newMousePosition.x
                , mouseY = newMousePosition.y
            }
