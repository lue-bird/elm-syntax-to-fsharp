module Program

[<EntryPoint>]
let main args =
    let input = stdin.ReadToEnd()
    stdout.Write(
        Elm.StringRope.toString(
            Elm.formatSingleModule_formatSingleModule
                (Elm.StringRopeOne input)
            )
    )
    0
