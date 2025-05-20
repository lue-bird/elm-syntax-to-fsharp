module Program

[<EntryPoint>]
let main (_arguments: array<string>) : int =
    let greeting: string =
        Elm.StringRope.toString
            (Elm.Hello_greet (Elm.StringRopeOne "world"))
    System.Console.WriteLine(greeting)

    0
