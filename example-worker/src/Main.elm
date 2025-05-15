port module Main exposing (main)

import Json.Encode

port portStdOutWrite : Json.Encode.Value -> Cmd event_
port portProcessExit : Json.Encode.Value -> Cmd event_

stdoutWrite : String -> Cmd event_
stdoutWrite output =
    portStdOutWrite (Json.Encode.string output)

processExit : Int -> Cmd event_
processExit code =
    portProcessExit (Json.Encode.int code)

type alias State =
    { userInput : String }

type Event
    = StdInRead String

type alias Flags =
    List String

main : Program Flags State Event
main =
    Platform.worker
        { init =
            \commandLineArguments ->
                ( { userInput = "" }
                , case commandLineArguments of
                    [ name ] ->
                        stdoutWrite ("Hi, " ++ name ++ "!\n")

                    _ ->
                        Cmd.batch
                            [ stdoutWrite "Good night, world.\n"
                            , processExit 1
                            ]
                )
        , update = \event state -> ( state, Cmd.none )
        , subscriptions = \state -> Sub.none
        }
