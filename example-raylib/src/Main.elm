port module Main exposing (main)

import Color exposing (Color)
import Duration exposing (Duration)
import Json.Decode
import Json.Encode
import Quantity
import Time


port portStdOutWrite : Json.Encode.Value -> Cmd event_


port portProcessExit : Json.Encode.Value -> Cmd event_


port portWindowTitleSet : Json.Encode.Value -> Cmd event_


port portWindowDimensionsSet : Json.Encode.Value -> Cmd event_


port portRender : Json.Encode.Value -> Cmd event_


port portFramePassed : (Json.Encode.Value -> event) -> Sub event


port portKeysPressed : (Json.Encode.Value -> event) -> Sub event


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


type ElementToRender
    = TextToRender TextToRender
    | RectangleToRender RectangleToRender


elementToRenderToJson : ElementToRender -> Json.Encode.Value
elementToRenderToJson elementToRender =
    case elementToRender of
        TextToRender textToRender ->
            Json.Encode.object
                [ ( "text", textToRender |> textToRenderToJson ) ]

        RectangleToRender rectangleToRender ->
            Json.Encode.object
                [ ( "rectangle", rectangleToRender |> rectangleToRenderToJson ) ]


type alias TextToRender =
    WithoutRecordConstructor
        { content : String
        , fontSize : Int
        , left : Int
        , top : Int
        , color : Color
        }


textToRenderToJson : TextToRender -> Json.Encode.Value
textToRenderToJson textToRender =
    Json.Encode.object
        [ ( "content", textToRender.content |> Json.Encode.string )
        , ( "fontSize", textToRender.fontSize |> Json.Encode.int )
        , ( "left", textToRender.left |> Json.Encode.int )
        , ( "top", textToRender.top |> Json.Encode.int )
        , ( "color", textToRender.color |> colorToJson )
        ]


type alias RectangleToRender =
    WithoutRecordConstructor
        { width : Int
        , height : Int
        , left : Int
        , top : Int
        , color : Color
        }


rectangleToRenderToJson : RectangleToRender -> Json.Encode.Value
rectangleToRenderToJson rectangleToRender =
    Json.Encode.object
        [ ( "width", rectangleToRender.width |> Json.Encode.int )
        , ( "height", rectangleToRender.height |> Json.Encode.int )
        , ( "left", rectangleToRender.left |> Json.Encode.int )
        , ( "top", rectangleToRender.top |> Json.Encode.int )
        , ( "color", rectangleToRender.color |> colorToJson )
        ]


render :
    { backgroundColor : Color
    , elements : List ElementToRender
    }
    -> Cmd event_
render toRender =
    portRender
        (Json.Encode.object
            [ ( "backgroundColor", toRender.backgroundColor |> colorToJson )
            , ( "elements", toRender.elements |> Json.Encode.list elementToRenderToJson )
            ]
        )


durationJsonDecoderFromSeconds : Json.Decode.Decoder Duration
durationJsonDecoderFromSeconds =
    Json.Decode.map Duration.seconds
        Json.Decode.float


framePassed : (Duration -> Event) -> Sub Event
framePassed onReadLine =
    portFramePassed
        (\value ->
            case value |> Json.Decode.decodeValue durationJsonDecoderFromSeconds of
                Ok duration ->
                    onReadLine duration

                Err error ->
                    PortEventFailedToDecode
                        { name = "portFramePassed"
                        , error = error
                        }
        )


keysPressed : (List Int -> Event) -> Sub Event
keysPressed onReadLine =
    portFramePassed
        (\value ->
            case value |> Json.Decode.decodeValue (Json.Decode.list Json.Decode.int) of
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
    , durationSinceWindowInit : Duration
    , bouncingLogoTopLeftPosition : { x : Float, y : Float }
    , bouncingLogoMovementPerSecond : { x : Float, y : Float }
    }


type Event
    = FramePassed Duration
    | PortEventFailedToDecode { name : String, error : Json.Decode.Error }


type alias Flags =
    -- command line arguments
    List String


main : Program Flags State Event
main =
    Platform.worker
        { init = init
        , update =
            \event state ->
                case event of
                    FramePassed newDurationSinceWindowInit ->
                        let
                            animationProgress : Float
                            animationProgress =
                                (newDurationSinceWindowInit
                                    |> Duration.inMilliseconds
                                    |> Basics.round
                                    |> Basics.remainderBy 6000
                                    |> Basics.toFloat
                                )
                                    / 6000

                            durationPreviousToCurrentFrame : Duration
                            durationPreviousToCurrentFrame =
                                newDurationSinceWindowInit
                                    |> Quantity.minus state.durationSinceWindowInit

                            bouncingLogoFutureTopLeftPosition : Position
                            bouncingLogoFutureTopLeftPosition =
                                state.bouncingLogoTopLeftPosition
                                    |> positionTranslateBy
                                        (state.bouncingLogoMovementPerSecond
                                            |> offsetsScaleBy
                                                (durationPreviousToCurrentFrame
                                                    |> Duration.inSeconds
                                                )
                                        )

                            bouncingLogoNewMovementPerSecond : Offsets
                            bouncingLogoNewMovementPerSecond =
                                { x =
                                    if
                                        ((bouncingLogoFutureTopLeftPosition.x |> Basics.round) < 0)
                                            || ((bouncingLogoFutureTopLeftPosition.x |> Basics.round)
                                                    + bouncingLogoWidth
                                                    > windowWidth
                                               )
                                    then
                                        -state.bouncingLogoMovementPerSecond.x

                                    else
                                        state.bouncingLogoMovementPerSecond.x
                                , y =
                                    if
                                        ((bouncingLogoFutureTopLeftPosition.y |> Basics.round) < 0)
                                            || ((bouncingLogoFutureTopLeftPosition.y |> Basics.round)
                                                    + bouncingLogoHeight
                                                    > windowHeight
                                               )
                                    then
                                        -state.bouncingLogoMovementPerSecond.y

                                    else
                                        state.bouncingLogoMovementPerSecond.y
                                }

                            bouncingLogoNewTopLeftPosition : Position
                            bouncingLogoNewTopLeftPosition =
                                if state.bouncingLogoMovementPerSecond == bouncingLogoNewMovementPerSecond then
                                    bouncingLogoFutureTopLeftPosition

                                else
                                    state.bouncingLogoTopLeftPosition
                                        |> positionTranslateBy
                                            (bouncingLogoNewMovementPerSecond
                                                |> offsetsScaleBy
                                                    (durationPreviousToCurrentFrame
                                                        |> Duration.inSeconds
                                                    )
                                            )
                        in
                        ( { state
                            | frameCount = state.frameCount + 1
                            , durationSinceWindowInit = newDurationSinceWindowInit
                            , bouncingLogoTopLeftPosition =
                                bouncingLogoNewTopLeftPosition
                            , bouncingLogoMovementPerSecond =
                                bouncingLogoNewMovementPerSecond
                          }
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
                            , elements =
                                [ RectangleToRender
                                    { width = bouncingLogoWidth
                                    , height = bouncingLogoHeight
                                    , left = bouncingLogoNewTopLeftPosition.x |> Basics.round
                                    , top = bouncingLogoNewTopLeftPosition.y |> Basics.round
                                    , color = Color.black
                                    }
                                , TextToRender
                                    { content =
                                        "Elm running on the big screen! Rendered "
                                            ++ (state.frameCount // 1000 |> String.fromInt)
                                            ++ "k times."
                                    , fontSize = 20
                                    , color = Color.white
                                    , left = 50
                                    , top = 12
                                    }
                                ]
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
        , subscriptions = subscriptions
        }


bouncingLogoWidth : Int
bouncingLogoWidth =
    200


bouncingLogoHeight : Int
bouncingLogoHeight =
    100


windowWidth : Int
windowWidth =
    800


windowHeight : Int
windowHeight =
    480


init : Flags -> ( State, Cmd Event )
init _ =
    ( { frameCount = 0
      , durationSinceWindowInit = Duration.seconds 0
      , bouncingLogoTopLeftPosition =
            { x = 0.0, y = 0.0 }
      , bouncingLogoMovementPerSecond =
            { x = 150, y = 150 }
      }
    , Cmd.batch
        [ stdOutWrite "Hello from elm which just got initialized!\n"
        , windowTitleSet "Yay raylib!"
        , windowDimensionsSet { width = windowWidth, height = windowHeight }
        ]
    )


subscriptions : State -> Sub Event
subscriptions state =
    framePassed FramePassed


type alias Position =
    { x : Float, y : Float }


type alias Offsets =
    { x : Float, y : Float }


positionTranslateBy : Offsets -> Position -> Position
positionTranslateBy offsets position =
    { x = position.x + offsets.x
    , y = position.y + offsets.y
    }


offsetsScaleBy : Float -> Offsets -> Offsets
offsetsScaleBy factor offsets =
    { x = offsets.x * factor
    , y = offsets.y * factor
    }


{-| https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/RecordWithoutConstructorFunction#RecordWithoutConstructorFunction
-}
type alias WithoutRecordConstructor recordType =
    recordType
