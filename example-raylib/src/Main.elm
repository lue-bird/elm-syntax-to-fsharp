port module Main exposing (main)

import Color exposing (Color)
import Duration exposing (Duration)
import Json.Decode
import Json.Encode
import Quantity
import Random
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


colorOpaqueRandom : Random.Generator Color
colorOpaqueRandom =
    Random.map (\h -> Color.hsl h 0.9 0.3)
        (Random.float 0 1)


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
    , bouncingLogoTopLeftPosition : Position
    , bouncingLogoMovementPerSecond : Offsets
    , bouncingLogoBounceCount : Int
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
                                    |> Basics.remainderBy 20000
                                    |> Basics.toFloat
                                )
                                    / 20000

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

                            logoBounceLeftOrRight : Bool
                            logoBounceLeftOrRight =
                                ((bouncingLogoFutureTopLeftPosition.x |> Basics.round) < 0)
                                    || ((bouncingLogoFutureTopLeftPosition.x |> Basics.round)
                                            + bouncingLogoWidth
                                            > windowWidth
                                       )

                            logoBounceTopOrBottom : Bool
                            logoBounceTopOrBottom =
                                ((bouncingLogoFutureTopLeftPosition.y |> Basics.round) < 0)
                                    || ((bouncingLogoFutureTopLeftPosition.y |> Basics.round)
                                            + bouncingLogoHeight
                                            > windowHeight
                                       )

                            logoBounceOnAnyWall : Bool
                            logoBounceOnAnyWall =
                                logoBounceLeftOrRight || logoBounceTopOrBottom

                            bouncingLogoNewMovementPerSecond : Offsets
                            bouncingLogoNewMovementPerSecond =
                                { x =
                                    if logoBounceLeftOrRight then
                                        -state.bouncingLogoMovementPerSecond.x

                                    else
                                        state.bouncingLogoMovementPerSecond.x
                                , y =
                                    if logoBounceTopOrBottom then
                                        -state.bouncingLogoMovementPerSecond.y

                                    else
                                        state.bouncingLogoMovementPerSecond.y
                                }

                            bouncingLogoNewTopLeftPosition : Position
                            bouncingLogoNewTopLeftPosition =
                                if logoBounceOnAnyWall then
                                    state.bouncingLogoTopLeftPosition
                                        |> positionTranslateBy
                                            (bouncingLogoNewMovementPerSecond
                                                |> offsetsScaleBy
                                                    (durationPreviousToCurrentFrame
                                                        |> Duration.inSeconds
                                                    )
                                            )

                                else
                                    bouncingLogoFutureTopLeftPosition

                            bouncingLogoNewBounceCount : Int
                            bouncingLogoNewBounceCount =
                                if logoBounceOnAnyWall then
                                    state.bouncingLogoBounceCount + 1

                                else
                                    state.bouncingLogoBounceCount
                        in
                        ( { state
                            | frameCount = state.frameCount + 1
                            , durationSinceWindowInit = newDurationSinceWindowInit
                            , bouncingLogoTopLeftPosition =
                                bouncingLogoNewTopLeftPosition
                            , bouncingLogoMovementPerSecond =
                                bouncingLogoNewMovementPerSecond
                            , bouncingLogoBounceCount =
                                bouncingLogoNewBounceCount
                          }
                        , render
                            { backgroundColor =
                                Color.hsl
                                    animationProgress
                                    1
                                    0.2
                            , elements =
                                [ TextToRender
                                    { content =
                                        "Elm running on the big screen!\nRendered "
                                            ++ (state.frameCount // 1000 |> String.fromInt)
                                            ++ "k times."
                                    , fontSize = 29
                                    , color = Color.white
                                    , left = 50
                                    , top = 100
                                    }
                                , RectangleToRender
                                    { width = bouncingLogoWidth
                                    , height = bouncingLogoHeight
                                    , left = bouncingLogoNewTopLeftPosition.x |> Basics.round
                                    , top = bouncingLogoNewTopLeftPosition.y |> Basics.round
                                    , color =
                                        Random.step colorOpaqueRandom
                                            (Random.initialSeed bouncingLogoNewBounceCount)
                                            |> Tuple.first
                                    }
                                , let
                                    bounceCountFontSize : Int
                                    bounceCountFontSize =
                                        Basics.min
                                            ((bouncingLogoHeight |> Basics.toFloat) * 0.75)
                                            (((bouncingLogoWidth |> Basics.toFloat)
                                                / (bounceCountContent |> String.length |> Basics.toFloat)
                                                * 2
                                             )
                                                * 0.75
                                            )
                                            |> Basics.round

                                    bounceCountContent : String
                                    bounceCountContent =
                                        bouncingLogoNewBounceCount |> String.fromInt
                                  in
                                  TextToRender
                                    { content = bounceCountContent
                                    , fontSize = bounceCountFontSize
                                    , left =
                                        bouncingLogoNewTopLeftPosition.x
                                            + ((bouncingLogoWidth |> Basics.toFloat) / 2)
                                            - (((bounceCountFontSize |> Basics.toFloat) / 2)
                                                * (bounceCountContent |> String.length |> Basics.toFloat)
                                                / 2
                                              )
                                            |> Basics.round
                                    , top =
                                        bouncingLogoNewTopLeftPosition.y
                                            + ((bouncingLogoHeight |> Basics.toFloat) / 2)
                                            - ((bounceCountFontSize |> Basics.toFloat) / 2)
                                            |> Basics.round
                                    , color = Color.white
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
      , bouncingLogoBounceCount = 0
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
