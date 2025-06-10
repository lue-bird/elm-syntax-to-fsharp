port module Main exposing (main)

import Color exposing (Color)
import Json.Decode
import Json.Encode


port portStdOutWrite : Json.Encode.Value -> Cmd event_


port portProcessExit : Json.Encode.Value -> Cmd event_


port portWindowTitleSet : Json.Encode.Value -> Cmd event_


port portWindowDimensionsSet : Json.Encode.Value -> Cmd event_


port portRender : Json.Encode.Value -> Cmd event_


port portFramePassed : (Json.Encode.Value -> event) -> Sub event


stdOutWrite : String -> Cmd event_
stdOutWrite output =
    portStdOutWrite (Json.Encode.string output)


processExit : Int -> Cmd event_
processExit code =
    portProcessExit (Json.Encode.int code)


windowTitleSet : String -> Cmd event_
windowTitleSet newTitle =
    portWindowTitleSet (Json.Encode.string newTitle)


windowDimensionsSet : { width : Int, height : Int } -> Cmd event_
windowDimensionsSet newDimensions =
    portWindowDimensionsSet
        (Json.Encode.object
            [ ( "width", newDimensions.width |> Json.Encode.int )
            , ( "height", newDimensions.height |> Json.Encode.int )
            ]
        )


colorToJson : Color -> Json.Encode.Value
colorToJson color =
    let
        components : { red : Float, green : Float, blue : Float, alpha : Float }
        components =
            color |> Color.toRgba
    in
    Json.Encode.object
        [ ( "red", components.red |> Json.Encode.float )
        , ( "green", components.green |> Json.Encode.float )
        , ( "blue", components.blue |> Json.Encode.float )
        , ( "alpha", components.alpha |> Json.Encode.float )
        ]


type alias TextToRender =
    { content : String
    , left : Int
    , top : Int
    , color : Color
    }


textToRenderToJson : TextToRender -> Json.Encode.Value
textToRenderToJson textToRender =
    Json.Encode.object
        [ ( "content", textToRender.content |> Json.Encode.string )
        , ( "left", textToRender.left |> Json.Encode.int )
        , ( "top", textToRender.top |> Json.Encode.int )
        , ( "color", textToRender.color |> colorToJson )
        ]


render :
    { backgroundColor : Color
    , text : TextToRender
    }
    -> Cmd event_
render toRender =
    portRender
        (Json.Encode.object
            [ ( "backgroundColor", toRender.backgroundColor |> colorToJson )
            , ( "text", toRender.text |> textToRenderToJson )
            ]
        )


framePassed : (() -> Event) -> Sub Event
framePassed onReadLine =
    portFramePassed
        (\value ->
            case value |> Json.Decode.decodeValue (Json.Decode.null ()) of
                Ok readLine ->
                    onReadLine readLine

                Err error ->
                    PortEventFailedToDecode
                        { name = "portFramePassed"
                        , error = error
                        }
        )


type alias State =
    { frameCount : Int
    }


type Event
    = FramePassed
    | PortEventFailedToDecode { name : String, error : Json.Decode.Error }


type alias Flags =
    -- command line arguments
    List String


main : Program Flags State Event
main =
    Platform.worker
        { init =
            \_ ->
                ( { frameCount = 0 }
                , Cmd.batch
                    [ stdOutWrite "Hello from elm which just got initialized!\n"
                    , windowTitleSet "Yay raylib!"
                    , windowDimensionsSet { width = 800, height = 480 }
                    ]
                )
        , update =
            \event state ->
                case event of
                    FramePassed ->
                        let
                            animationProgress : Float
                            animationProgress =
                                (state.frameCount
                                    |> Basics.remainderBy 20000
                                    |> Basics.toFloat
                                )
                                    / 20000
                                    / 2
                        in
                        ( { state | frameCount = state.frameCount + 1 }
                        , render
                            { backgroundColor =
                                Color.fromRgba
                                    { red =
                                        (1
                                            - (animationProgress
                                                |> Basics.turns
                                                |> Basics.sin
                                              )
                                        )
                                            * 0.5
                                    , green = 0
                                    , blue =
                                        (animationProgress
                                            |> Basics.turns
                                            |> Basics.sin
                                        )
                                            * 0.3
                                    , alpha = 0
                                    }
                            , text =
                                { content =
                                    "Elm running on the big screen! Rendered "
                                        ++ (state.frameCount // 1000 |> String.fromInt)
                                        ++ "k times."
                                , color = Color.white
                                , left = 50
                                , top = 12
                                }
                            }
                        )

                    PortEventFailedToDecode portError ->
                        ( state
                        , Cmd.batch
                            [ stdOutWrite
                                ("Failed to decode event of port "
                                    ++ portError.name
                                    ++ ": "
                                    ++ (portError.error |> Json.Decode.errorToString)
                                    ++ ".\n"
                                )
                            , processExit 1
                            ]
                        )
        , subscriptions =
            \state ->
                framePassed (\() -> FramePassed)
        }
