module Program

let tst: {| Name: string |} -> string = _.Name

[<EntryPoint>]
let main args =
    let input = stdin.ReadToEnd()
    stdout.Write(Elm.formatSingleModule_formatSingleModule input)
    0
