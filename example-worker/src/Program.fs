module Program

[<Struct>]
type ElmSubscriptionSingleRunningInfo<'event> =
    { AbortController: System.Threading.CancellationTokenSource
      Sub: Elm.PlatformSub_SubSingle<'event> }

[<Struct>]
type PortConfig<'decodedValue> =
    { Name: string
      ValueDecoder: Elm.JsonDecode_Decoder<'decodedValue>
      Act: 'decodedValue -> unit }

[<Struct>]
type PortRunner =
    { Name: string
      Run: System.Text.Json.Nodes.JsonNode -> unit }

let toPortRunner (config: PortConfig<'decodedValue>) : PortRunner =
    { Name = config.Name
      Run =
        fun value ->
            match Elm.JsonDecode_decodeValue config.ValueDecoder value with
            | Error error ->
                stdout.Write(
                    "Error: failed to decode port "
                    + config.Name
                    + " value "
                    + Elm.StringRope.toString (
                        Elm.JsonDecode_errorToString error
                    )
                    + "\n"
                )
            | Ok decodedValue -> config.Act decodedValue }

let performElmCommandSingle
    (commandSingle: Elm.PlatformCmd_CmdSingle<'event>)
    : unit =
    match commandSingle with
    | Elm.PlatformCmd_PortOutgoing { Name = portOutgoingName
                                     Value = value } ->
        let maybeActionToApply =
            List.tryFind
                (fun (portRunner: PortRunner) ->
                    portOutgoingName = portRunner.Name)
                [ toPortRunner
                      { Name = "portStdOutWrite"
                        ValueDecoder = Elm.JsonDecode_string
                        Act =
                          fun output ->
                              stdout.Write(Elm.StringRope.toString output) }
                  toPortRunner
                      { Name = "portProcessExit"
                        ValueDecoder = Elm.JsonDecode_int
                        Act =
                          fun output -> System.Environment.Exit(int output) } ]

        match maybeActionToApply with
        | None ->
            stdout.Write(
                "Error: unknown port name " + portOutgoingName + "\n"
            )
        | Some actionToApply -> actionToApply.Run value

let startElmSubscriptionSingle
    (subscriptionSingle: ElmSubscriptionSingleRunningInfo<'event>)
    (onEvent: System.Text.Json.Nodes.JsonNode -> Async<unit>)
    : Async<unit> =
    match subscriptionSingle.Sub with
    | Elm.PlatformSub_PortIncoming { Name = portIncomingName; OnValue = _ } ->
        match portIncomingName with
        | "portStdInReadLine" ->
            async {
                let inputReader =
                    new System.IO.StreamReader(
                        System.Console.OpenStandardInput(),
                        System.Console.InputEncoding
                    )

                let abortSignal = subscriptionSingle.AbortController.Token

                while not abortSignal.IsCancellationRequested do
                    try
                        let! readLine =
                            Async.AwaitTask(
                                inputReader
                                    .ReadLineAsync(abortSignal)
                                    .AsTask()
                            )

                        if readLine <> null then
                            let! () =
                                onEvent (
                                    Elm.JsonEncode_string(
                                        Elm.StringRopeOne readLine
                                    )
                                )

                            ()
                    with :? System.Threading.Tasks.TaskCanceledException ->
                        ()
            }
        | unknownPortName ->
            async {
                stdout.Write(
                    "Error: unknown port name " + unknownPortName + "\n"
                )
            }

////

let performElmCmd (commands: Elm.PlatformCmd_Cmd<'event>) : unit =
    List.iter
        (fun commandSingle -> performElmCommandSingle commandSingle)
        commands

let elmSubscriptionSingleToId
    (subscriptionSingle: Elm.PlatformSub_SubSingle<'event>)
    : string =
    match subscriptionSingle with
    | Elm.PlatformSub_PortIncoming portIncoming -> portIncoming.Name

let performElmSub
    (subSinglesAbortable:
        Map<string, ElmSubscriptionSingleRunningInfo<'event>>)
    (onEvent: string -> System.Text.Json.Nodes.JsonNode -> Async<unit>)
    : Async<unit> =
    Async.Ignore(
        Async.Parallel(
            Seq.map
                (fun
                    (subSingleAbortable:
                        System.Collections.Generic.KeyValuePair<
                            string,
                            ElmSubscriptionSingleRunningInfo<'event>
                         >) ->
                    startElmSubscriptionSingle
                        subSingleAbortable.Value
                        (fun value -> onEvent subSingleAbortable.Key value))
                subSinglesAbortable
        )
    )

let elmSubToAbortableSingles
    (sub: Elm.PlatformSub_Sub<'event>)
    : Map<string, ElmSubscriptionSingleRunningInfo<'event>> =
    Map.ofSeq (
        Seq.map
            (fun subscriptionSingle ->
                (elmSubscriptionSingleToId subscriptionSingle,
                 { AbortController =
                     new System.Threading.CancellationTokenSource()
                   Sub = subscriptionSingle }))
            sub
    )

[<EntryPoint>]
let main (args: array<string>) : int =
    let struct (initialElmState, initialElmCommands) =
        Elm.Main_main.Init(Seq.toList (Seq.map Elm.StringRopeOne args))

    let mutable currentElmState = initialElmState
    let initialElmSub = Elm.Main_main.Subscriptions currentElmState

    let mutable currentElmRunningSubs =
        elmSubToAbortableSingles initialElmSub

    let rec onEvent
        (portName: string)
        (value: System.Text.Json.Nodes.JsonNode)
        : Async<unit> =
        match Map.tryFind portName currentElmRunningSubs with
        | None -> // updated sub does not listen to this event
            async { () }
        | Some associatedRunningElmSubscriptionSingle ->
            let event =
                match associatedRunningElmSubscriptionSingle.Sub with
                | Elm.PlatformSub_PortIncoming portIncoming ->
                    portIncoming.OnValue value

            let struct (nextElmState, elmCommands) =
                Elm.Main_main.Update event currentElmState

            currentElmState <- nextElmState
            performElmCmd elmCommands
            let updatedElmSub = Elm.Main_main.Subscriptions currentElmState

            let updatedElmRunningSubs =
                // TODO: only create abort controllers for added subscriptions
                elmSubToAbortableSingles updatedElmSub

            let elmSubSinglesToAdd =
                Elm.Dict_diff updatedElmRunningSubs currentElmRunningSubs

            let elmRunningSubsToRemove =
                Elm.Dict_diff currentElmRunningSubs updatedElmRunningSubs

            Map.iter
                (fun _ toAbort ->
                    toAbort.AbortController.Cancel()
                    toAbort.AbortController.Dispose())
                elmRunningSubsToRemove

            currentElmRunningSubs <-
                Map.map
                    (fun updatedSubId updatedSubRunningInfo ->
                        // preserve old abort controllers
                        // but still keep the new listeners
                        match
                            Map.tryFind updatedSubId currentElmRunningSubs
                        with
                        | None -> updatedSubRunningInfo
                        | Some currentSubWithUpdatedIdRunningInfo ->
                            { AbortController =
                                currentSubWithUpdatedIdRunningInfo.AbortController
                              Sub = updatedSubRunningInfo.Sub })
                    updatedElmRunningSubs

            performElmSub elmSubSinglesToAdd onEvent

    performElmCmd initialElmCommands
    Async.RunSynchronously(performElmSub currentElmRunningSubs onEvent)

    0
