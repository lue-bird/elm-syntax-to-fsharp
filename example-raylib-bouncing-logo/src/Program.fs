module Program

open Raylib_cs


let elmColorToRaylib (elmColor: Elm.Color_Color) : Color =
    let components = Elm.Color_toRgba elmColor

    new Color(
        float32 components.Red,
        float32 components.Green,
        float32 components.Blue,
        float32 components.Alpha
    )

[<Struct>]
type WindowDimensions = { Width: int; Height: int }


let performElmCommandSingle
    (commandSingle: Elm.PlatformCmd_CmdSingle<'event>)
    : unit =
    match commandSingle with
    | Elm.PlatformCmd_PortOutgoing { Name = _; Value = portValue } ->
        match Elm.Codec_decodeValue Elm.Main_portFromElmCodec portValue with
        | Error error ->
            stdout.Write(
                "Error: port value from elm had an unexpected shape: "
                + Elm.StringRope.toString (
                    Elm.JsonDecode_errorToString error
                )
                + "\n"
            )
        | Ok decodedPortFromElm ->
            match decodedPortFromElm with
            | Elm.Main_PortStdOutWrite output ->
                stdout.Write(Elm.StringRope.toString output)
            | Elm.Main_PortProcessExit code ->
                System.Environment.Exit(int code)
            | Elm.Main_PortWindowTitleSet newTitle ->
                Raylib.SetWindowTitle(Elm.StringRope.toString newTitle)
            | Elm.Main_PortWindowDimensionsSet newDimensions ->
                Raylib.SetWindowSize(
                    int newDimensions.Width,
                    int newDimensions.Height
                )
            | Elm.Main_PortRender toRender ->
                Raylib.BeginDrawing()

                Raylib.ClearBackground(
                    elmColorToRaylib toRender.BackgroundColor
                )

                toRender.Elements
                |> List.iter (fun elementToRender ->
                    match elementToRender with
                    | Elm.Main_TextToRender textToRender ->
                        Raylib.DrawText(
                            Elm.StringRope.toString textToRender.Content,
                            int textToRender.Left,
                            int textToRender.Top,
                            int textToRender.FontSize,
                            elmColorToRaylib textToRender.Color
                        )
                    | Elm.Main_RectangleToRender rectangleToRender ->
                        Raylib.DrawRectangle(
                            int rectangleToRender.Left,
                            int rectangleToRender.Top,
                            int rectangleToRender.Width,
                            int rectangleToRender.Height,
                            elmColorToRaylib rectangleToRender.Color
                        ))

                Raylib.EndDrawing()

let performElmCmd (commands: Elm.PlatformCmd_Cmd<'event>) : unit =
    List.iter
        (fun commandSingle -> performElmCommandSingle commandSingle)
        commands


let cBoolTrue: CBool = CBool true

let cBoolToBool (cBool: CBool) : bool = cBool = cBoolTrue

let pressedKeys () : array<int> =
    let mutable pressedKeysArray: System.Collections.Generic.List<int> =
        new System.Collections.Generic.List<int>()

    let mutable isDone: bool = false

    while not isDone do
        let nextPressedKey = Raylib.GetKeyPressed()

        if nextPressedKey = 0 then
            isDone <- true
        else
            pressedKeysArray.Add(nextPressedKey)

    pressedKeysArray.ToArray()

[<EntryPoint>]
let main args =
    let struct (initialElmState, initialElmCommands) =
        Elm.Main_main.Init(Seq.toList (Seq.map Elm.StringRope.fromString args))

    let mutable currentElmState = initialElmState

    Raylib.InitWindow(1, 1, "")
    performElmCmd initialElmCommands

    let onEvent event : unit =
        let struct (nextElmState, elmCommands) =
            Elm.Main_main.Update event currentElmState

        currentElmState <- nextElmState
        performElmCmd elmCommands

    while not (cBoolToBool (Raylib.WindowShouldClose())) do
        Elm.Main_main.Subscriptions currentElmState
        |> List.iter (fun subscriptionSingle ->
            match subscriptionSingle with
            | Elm.PlatformSub_PortIncoming portIncoming ->
                match portIncoming.Name with
                | "portFramePassed" ->
                    onEvent (
                        portIncoming.OnValue(
                            Elm.JsonEncode_float(Raylib.GetTime())
                        )
                    )
                | "portKeysPressed" ->
                    onEvent (
                        portIncoming.OnValue(
                            Elm.JsonEncode_array
                                (fun (code: int) ->
                                    Elm.JsonEncode_int(int64 code))
                                (pressedKeys ())
                        )
                    )
                | unknownPortName ->
                    stdout.Write(
                        "Error: unknown port name "
                        + unknownPortName
                        + "\n"
                    ))

    Raylib.CloseWindow()

    0
