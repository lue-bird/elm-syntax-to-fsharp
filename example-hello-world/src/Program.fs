module Program

[<EntryPoint>]
let main (_arguments: array<string>) : int =
    let greeting: string =
        Elm.StringRope.toString (Elm.Hello_greet(Elm.StringRope.fromString "world"))

    System.Console.WriteLine greeting

    0
