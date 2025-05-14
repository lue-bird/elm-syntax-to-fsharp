module Program

type PortConfig<'decodedValue> =
    { Name: string
    ; ValueDecoder: Elm.JsonDecode_Decoder<'decodedValue>
    ; Act: 'decodedValue -> unit
    }
type PortRunner =
    { Name: string
    ; Run: System.Text.Json.Nodes.JsonNode -> unit
    }
let toPortRunner (config: PortConfig<'decodedValue>) : PortRunner =
    { Name = config.Name
    ; Run =
        fun value ->
            match Elm.JsonDecode_decodeValue config.ValueDecoder value with
            | Error error ->
                stdout.Write(
                    "Error: failed to decode port "
                        + config.Name
                        + " value "
                        + Elm.StringRope.toString (Elm.JsonDecode_errorToString error)
                        + "\n"
                )
            | Ok decodedValue ->
                config.Act decodedValue
    }
let performElmCommandSingle (commandSingle: Elm.PlatformCmd_CmdSingle<'event>) : unit =
    match commandSingle with
    | Elm.PlatformCmd_PortOutgoing { Name = portOutgoingName; Value = value } ->
        let maybeActionToApply =
            List.tryFind
                (fun (portRunner: PortRunner) ->
                    portOutgoingName = portRunner.Name
                )
                [ toPortRunner
                    { Name = "portStdOutWrite"
                    ; ValueDecoder = Elm.JsonDecode_string
                    ; Act =
                        fun output ->
                            stdout.Write(Elm.StringRope.toString output)
                    }
                ; toPortRunner
                    { Name = "portProcessExit"
                    ; ValueDecoder = Elm.JsonDecode_int
                    ; Act =
                        fun output ->
                            System.Environment.Exit(int output)
                    }
                ]
        
        match maybeActionToApply with
        | None ->
            stdout.Write("Error: unknown port name " + portOutgoingName + "\n")
        | Some actionToApply ->
            actionToApply.Run value
        
let performElmCmd (commands: Elm.PlatformCmd_Cmd<'event>) : unit =
    List.iter
        (fun commandSingle -> performElmCommandSingle commandSingle)
        commands

[<EntryPoint>]
let main args =
    let (struct( initialElmState, initialElmCommands )) =
        Elm.Main_main.Init ()
    let mutable elmState = initialElmState

    performElmCmd initialElmCommands

    // while true do
    //     let (struct( nextElmState, elmCommands )) =
    //         Elm.Main_main.Update event elmState
    //     elmState <- nextElmState
    //     performElmCmd elmCommands
    // done

    0
