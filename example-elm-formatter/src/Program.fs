﻿module Program

[<EntryPoint>]
let main args =
    let input = stdin.ReadToEnd()
    stdout.Write(
        Elm.StringRope.toString(
            Elm.FormatSingleModule_formatSingleModule
                (Elm.StringRope.fromString input)
            )
    )
    0
