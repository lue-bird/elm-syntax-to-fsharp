port module Main exposing (main)

import Json.Encode
import Json.Decode


port portStdOutWrite : Json.Encode.Value -> Cmd event_
port portProcessExit : Json.Encode.Value -> Cmd event_
port portStdInReadLine : (Json.Encode.Value -> event) -> Sub event

stdOutWrite : String -> Cmd event_
stdOutWrite output =
    portStdOutWrite (Json.Encode.string output)

processExit : Int -> Cmd event_
processExit code =
    portProcessExit (Json.Encode.int code)

stdInReadLine : Sub Event
stdInReadLine =
    portStdInReadLine
        (\value ->
            case value |> Json.Decode.decodeValue Json.Decode.string of
                Ok readLine ->
                    StdInLineRead readLine

                Err error ->
                    PortEventFailedToDecode
                        { name = "portStdInReadLine"
                        , error = error
                        }
        )


type alias State =
    { name : Maybe String }

type Event
    = StdInLineRead String
    | PortEventFailedToDecode { name : String, error : Json.Decode.Error }

type alias Flags =
    List String

main : Program Flags State Event
main =
    Platform.worker
        { init =
            \commandLineArguments ->
                case commandLineArguments of
                    [ name ] ->
                        ( { name = Just name }
                        , stdOutWrite ("Hi, " ++ name ++ "!\n")
                        )

                    _ ->
                        ( { name = Nothing }
                        , stdOutWrite ("What's your name?  > ")
                        )
        , update =
            \event state ->
                case event of
                    StdInLineRead readLine ->
                        ( { name = Just readLine }
                        , stdOutWrite ("Hi, " ++ readLine ++ "!\n")
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
                case state.name of
                    Nothing ->
                        stdInReadLine
                    
                    Just _ ->
                        Sub.none
        }
