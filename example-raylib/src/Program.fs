module Program

open Raylib_cs

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

let JsonDecodeFloat32: Elm.JsonDecode_Decoder<float32> =
    Elm.JsonDecode_map float32 Elm.JsonDecode_float

let JsonDecodeInt32: Elm.JsonDecode_Decoder<int32> =
    Elm.JsonDecode_map int32 Elm.JsonDecode_int

let jsonDecodeColor: Elm.JsonDecode_Decoder<Color> =
    Elm.JsonDecode_map4
        (fun
            (red: float32)
            (green: float32)
            (blue: float32)
            (alpha: float32) -> new Color(red, green, blue, alpha))
        (Elm.JsonDecode_fieldRaw "red" JsonDecodeFloat32)
        (Elm.JsonDecode_fieldRaw "green" JsonDecodeFloat32)
        (Elm.JsonDecode_fieldRaw "blue" JsonDecodeFloat32)
        (Elm.JsonDecode_fieldRaw "alpha" JsonDecodeFloat32)

[<Struct>]
type WindowDimensions = { Width: int; Height: int }

let jsonDecodeWindowDimensions =
    Elm.JsonDecode_map2
        (fun (width: int) (height: int) ->
            { Width = width; Height = height })
        (Elm.JsonDecode_fieldRaw "width" JsonDecodeInt32)
        (Elm.JsonDecode_fieldRaw "height" JsonDecodeInt32)

[<Struct>]
type TextToRender =
    { Content: string
      Left: int
      Top: int
      Color: Color }

let jsonDecodeTextToRender: Elm.JsonDecode_Decoder<TextToRender> =
    Elm.JsonDecode_map4
        (fun content left top color ->
            { Content = content
              Left = left
              Top = top
              Color = color })
        (Elm.JsonDecode_fieldRaw "content" Elm.JsonDecode_stringRaw)
        (Elm.JsonDecode_fieldRaw "left" JsonDecodeInt32)
        (Elm.JsonDecode_fieldRaw "top" JsonDecodeInt32)
        (Elm.JsonDecode_fieldRaw "color" jsonDecodeColor)

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
                          fun output -> System.Environment.Exit(int output) }
                  toPortRunner
                      { Name = "portWindowTitleSet"
                        ValueDecoder = Elm.JsonDecode_string
                        Act =
                          fun newTitle ->
                              Raylib.SetWindowTitle(
                                  Elm.StringRope.toString newTitle
                              ) }
                  toPortRunner
                      { Name = "portWindowDimensionsSet"
                        ValueDecoder = jsonDecodeWindowDimensions
                        Act =
                          fun newDimensions ->
                              Raylib.SetWindowSize(
                                  newDimensions.Width,
                                  newDimensions.Height
                              ) }
                  toPortRunner
                      { Name = "portRender"
                        ValueDecoder =
                          Elm.JsonDecode_map2
                              (fun backgroundColor text ->
                                  {| BackgroundColor = backgroundColor
                                     Text = text |})
                              (Elm.JsonDecode_fieldRaw
                                  "backgroundColor"
                                  jsonDecodeColor)
                              (Elm.JsonDecode_fieldRaw
                                  "text"
                                  jsonDecodeTextToRender)
                        Act =
                          fun toRender ->
                              Raylib.BeginDrawing()

                              Raylib.ClearBackground(
                                  toRender.BackgroundColor
                              )

                              Raylib.DrawText(
                                  toRender.Text.Content,
                                  toRender.Text.Left,
                                  toRender.Text.Top,
                                  20,
                                  Color.White
                              )

                              Raylib.EndDrawing() } ]

        match maybeActionToApply with
        | None ->
            stdout.Write(
                "Error: unknown port name " + portOutgoingName + "\n"
            )
        | Some actionToApply -> actionToApply.Run value

let performElmCmd (commands: Elm.PlatformCmd_Cmd<'event>) : unit =
    List.iter
        (fun commandSingle -> performElmCommandSingle commandSingle)
        commands


let cBoolTrue: CBool = new CBool(true)

let cBoolToBool (cBool: CBool) : bool =
    Raylib.WindowShouldClose() = cBoolTrue

[<EntryPoint>]
let main args =
    let struct (initialElmState, initialElmCommands) =
        Elm.Main_main.Init(Seq.toList (Seq.map Elm.StringRopeOne args))

    let mutable currentElmState = initialElmState

    Raylib.InitWindow(1, 1, "")
    performElmCmd initialElmCommands

    while not (cBoolToBool (Raylib.WindowShouldClose())) do
        Elm.Main_main.Subscriptions currentElmState
        |> List.iter (fun subscriptionSingle ->
            match subscriptionSingle with
            | Elm.PlatformSub_PortIncoming portIncoming ->
                match portIncoming.Name with
                | "portFramePassed" ->
                    let event = portIncoming.OnValue Elm.JsonEncode_null

                    let struct (nextElmState, elmCommands) =
                        Elm.Main_main.Update event currentElmState

                    currentElmState <- nextElmState
                    performElmCmd elmCommands
                | unknownPortName ->
                    stdout.Write(
                        "Error: unknown port name "
                        + unknownPortName
                        + "\n"
                    ))

    Raylib.CloseWindow()

    0
