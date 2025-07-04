module Program

[<Struct>]
type PortConfig =
    { Name: string
      ValueDecoder: Elm.JsonDecode_Decoder<unit> }

let outgoingPortConfigs: List<PortConfig> =
    [ { Name = "portStdOutWrite"
        ValueDecoder =
          Elm.JsonDecode_map
              (fun (output: string) -> stdout.Write output)
              Elm.JsonDecode_stringRaw }

      { Name = "portProcessExit"
        ValueDecoder =
          Elm.JsonDecode_map
              (fun output -> System.Environment.Exit(int output))
              Elm.JsonDecode_int } ]

[<Struct>]
type ElmSubscriptionSingleRunningInfo<'event> =
    { AbortController: System.Threading.CancellationTokenSource
      Sub: Elm.PlatformSub_SubSingle<'event> }

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
                                    Elm.JsonEncode_stringRaw readLine
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

let performElmCommandSingle
    (commandSingle: Elm.PlatformCmd_CmdSingle<'event>)
    : unit =
    match commandSingle with
    | Elm.PlatformCmd_PortOutgoing { Name = portOutgoingName
                                     Value = value } ->
        let maybeActionToApply: option<PortConfig> =
            List.tryFind
                (fun (portRunner: PortConfig) ->
                    portOutgoingName = portRunner.Name)
                outgoingPortConfigs

        match maybeActionToApply with
        | None ->
            stdout.Write(
                "Error: unknown port name " + portOutgoingName + "\n"
            )
        | Some actionToApply ->
            match
                Elm.JsonDecode_decodeValue actionToApply.ValueDecoder value
            with
            | Error error ->
                stdout.Write(
                    "Error: failed to decode port "
                    + actionToApply.Name
                    + " value "
                    + Elm.StringRope.toString (
                        Elm.JsonDecode_errorToString error
                    )
                    + "\n"
                )
            | Ok() -> ()

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
        Elm.Main_main.Init(Seq.toList (Seq.map Elm.StringRope.fromString args))

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
