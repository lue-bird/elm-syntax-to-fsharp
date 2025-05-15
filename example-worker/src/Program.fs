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


let startElmSubscriptionSingle
    (subscriptionSince: Elm.PlatformSub_SubSingle<'event>)
    (onEvent: 'event -> unit)
    : Async<unit> =
    match subscriptionSince with
    | Elm.PlatformSub_PortIncoming { Name = portIncomingName; OnValue = onValue } ->
        match portIncomingName with
        | "portStdInReadLine" ->
            async {
                let inputReader =
                    new System.IO.StreamReader(
                        System.Console.OpenStandardInput(),
                        System.Console.InputEncoding
                    )
                let! readLine = Async.AwaitTask (inputReader.ReadLineAsync())
                onEvent(onValue(Elm.JsonEncode_string (Elm.StringRopeOne readLine)))
                inputReader.Close()
            }
        | unknownPortName ->
            async {
                stdout.Write("Error: unknown port name " + unknownPortName + "\n")
            }
let performElmSub
    (subscriptions: Elm.PlatformSub_Sub<'event>)
    (onEvent: 'event -> unit)
    : Async<unit> =
    Async.Ignore
        (Async.Parallel
            (Seq.map
                (fun subscriptionSingle ->
                    startElmSubscriptionSingle subscriptionSingle onEvent
                )
                subscriptions
            )
        )

[<EntryPoint>]
let main args =
    let (struct( initialElmState, initialElmCommands )) =
        Elm.Main_main.Init (Seq.toList (Seq.map Elm.StringRopeOne args))
    let mutable currentElmState = initialElmState

    let rec onEvent event =
        let (struct( nextElmState, elmCommands )) =
            Elm.Main_main.Update event currentElmState
        currentElmState <- nextElmState
        performElmCmd elmCommands
        let sub = Elm.Main_main.Subscriptions currentElmState
        // TODO cancel those not present in the new sub
        Async.Start (performElmSub sub onEvent)

    performElmCmd initialElmCommands
    let sub = Elm.Main_main.Subscriptions currentElmState
    Async.RunSynchronously (performElmSub sub onEvent)

    0
