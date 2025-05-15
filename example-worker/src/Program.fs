module Program

let MapDiff
    (baseMap: Map<'key, 'a>)
    (mapWithKeysToRemove: Map<'key, 'b>)
    : Map<'key, 'a> =
    Map.fold (fun soFar k _ -> Map.remove k soFar) baseMap mapWithKeysToRemove

[<Struct>]
type PortConfig<'decodedValue> =
    { Name: string
    ; ValueDecoder: Elm.JsonDecode_Decoder<'decodedValue>
    ; Act: 'decodedValue -> unit
    }
[<Struct>]
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


[<Struct>]
type ElmSubscriptionSingleId =
    | ElmSubscriptionSingleIdPortIncoming of string
let elmSubscriptionSingleToId
    (subscriptionSingle: Elm.PlatformSub_SubSingle<'event>)
    : ElmSubscriptionSingleId =
    match subscriptionSingle with
    | Elm.PlatformSub_PortIncoming(portIncoming) ->
        ElmSubscriptionSingleIdPortIncoming portIncoming.Name
let elmSubscriptionSingleToOnValue
    (subscriptionSingle: Elm.PlatformSub_SubSingle<'event>)
    : System.Text.Json.Nodes.JsonNode -> 'event =
    match subscriptionSingle with
    | Elm.PlatformSub_PortIncoming(portIncoming) ->
        portIncoming.OnValue


let startElmSubscriptionSingle
    (subscriptionSingle: Elm.PlatformSub_SubSingle<'event>)
    (onEvent: System.Text.Json.Nodes.JsonNode -> unit)
    : Async<unit> =
    match subscriptionSingle with
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
                onEvent (Elm.JsonEncode_string (Elm.StringRopeOne readLine))
                inputReader.Close()
            }
        | unknownPortName ->
            async {
                stdout.Write("Error: unknown port name " + unknownPortName + "\n")
            }
let performElmSub
    (sub: Elm.PlatformSub_Sub<'event>)
    (onEvent: ElmSubscriptionSingleId -> System.Text.Json.Nodes.JsonNode -> unit)
    : Async<unit> =
    Async.Ignore
        (Async.Parallel
            (Seq.map
                (fun subscriptionSingle ->
                    let id = elmSubscriptionSingleToId subscriptionSingle
                    startElmSubscriptionSingle subscriptionSingle
                        (fun value -> onEvent id value)
                )
                sub
            )
        )

[<Struct>]
type ElmSubscriptionSingleRunningInfo<'event> =
    { AbortController: System.Threading.CancellationTokenSource
    ; OnValue: System.Text.Json.Nodes.JsonNode -> 'event
    }
let elmSubToAbortableSingles
    (sub: Elm.PlatformSub_Sub<'event>)
    : Map<ElmSubscriptionSingleId, ElmSubscriptionSingleRunningInfo<'event>> =
    Map.ofSeq
        (Seq.map
            (fun subscriptionSingle ->
                ( elmSubscriptionSingleToId subscriptionSingle
                , { AbortController = new System.Threading.CancellationTokenSource()
                  ; OnValue = elmSubscriptionSingleToOnValue subscriptionSingle
                  }
                )
            )
            sub
        )

[<EntryPoint>]
let main args =
    let (struct( initialElmState, initialElmCommands )) =
        Elm.Main_main.Init (Seq.toList (Seq.map Elm.StringRopeOne args))
    let mutable currentElmState = initialElmState
    let initialElmSub = Elm.Main_main.Subscriptions currentElmState
    let mutable currentElmRunningSubs = elmSubToAbortableSingles initialElmSub

    let rec onEvent (id: ElmSubscriptionSingleId) (value: System.Text.Json.Nodes.JsonNode): unit =
        match Map.tryFind id currentElmRunningSubs with
        | // updated sub does not listen to this event
          None ->
            ()
        | Some(associatedRunningElmSubscriptionSingle) ->
            let event = associatedRunningElmSubscriptionSingle.OnValue value
            let (struct( nextElmState, elmCommands )) =
                Elm.Main_main.Update event currentElmState
            currentElmState <- nextElmState
            performElmCmd elmCommands
            let updatedElmSub = Elm.Main_main.Subscriptions currentElmState
            let updatedElmRunningSubs = elmSubToAbortableSingles updatedElmSub

            let elmSubSinglesToAdd =
                List.filter
                    (fun updatedSubSingle ->
                        not
                            (Map.containsKey
                                (elmSubscriptionSingleToId updatedSubSingle)
                                currentElmRunningSubs
                            )
                    )
                    updatedElmSub
            let elmRunningSubsToRemove =
                MapDiff currentElmRunningSubs updatedElmRunningSubs
            
            Map.iter
                (fun _ toAbort -> toAbort.AbortController.Cancel())
                elmRunningSubsToRemove
            
            Async.Start (performElmSub elmSubSinglesToAdd onEvent)
            currentElmRunningSubs <- updatedElmRunningSubs
    
    performElmCmd initialElmCommands
    Async.RunSynchronously(performElmSub initialElmSub onEvent)

    0
