port module Main exposing (main)

import Json.Decode
import Json.Encode


port portStdOutWrite : Json.Encode.Value -> Cmd event_


port portProcessExit : Json.Encode.Value -> Cmd event_


port portStdInReadLine : (Json.Encode.Value -> event) -> Sub event


stdOutWrite : String -> Cmd event_
stdOutWrite output =
    portStdOutWrite (Json.Encode.string output)


processExit : Int -> Cmd event_
processExit code =
    portProcessExit (Json.Encode.int code)


stdInReadLine : (String -> Event) -> Sub Event
stdInReadLine onReadLine =
    portStdInReadLine
        (\value ->
            case value |> Json.Decode.decodeValue Json.Decode.string of
                Ok readLine ->
                    onReadLine readLine

                Err error ->
                    PortEventFailedToDecode
                        { name = "portStdInReadLine"
                        , error = error
                        }
        )


type alias State =
    { name : Maybe String
    , title : Maybe String
    }


type Event
    = StdInLineReadName String
    | StdInLineReadTitle String
    | PortEventFailedToDecode { name : String, error : Json.Decode.Error }


type alias Flags =
    List String


main : Program Flags State Event
main =
    Platform.worker
        { init =
            \commandLineArguments ->
                case commandLineArguments of
                    [ name, title ] ->
                        ( { name = Just name, title = Just title }
                        , stdOutWrite
                            ("Welcome, "
                                ++ name
                                ++ ", the "
                                ++ title
                                ++ ". Nice to meet ya!"
                            )
                        )

                    [ name ] ->
                        ( { name = Just name, title = Nothing }
                        , stdOutWrite
                            ("Hi, "
                                ++ name
                                ++ ". Which title should I assign to you?  > "
                            )
                        )

                    _ ->
                        ( { name = Nothing, title = Nothing }
                        , stdOutWrite "What's your name?  > "
                        )
        , update =
            \event state ->
                case event of
                    StdInLineReadName name ->
                        ( { state | name = Just name }
                        , stdOutWrite
                            ("Hi, "
                                ++ name
                                ++ ". Which title should I assign to you?  > "
                            )
                        )

                    StdInLineReadTitle title ->
                        ( { state | title = Just title }
                        , stdOutWrite
                            ("Got it. "
                                ++ Maybe.withDefault "unnamed" state.name
                                ++ ", the "
                                ++ title
                                ++ ". Nice to meet ya!\n"
                            )
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
                case ( state.name, state.title ) of
                    ( Just _, Just _ ) ->
                        Sub.none

                    ( Nothing, _ ) ->
                        stdInReadLine StdInLineReadName

                    ( Just _, Nothing ) ->
                        stdInReadLine StdInLineReadTitle
        }
