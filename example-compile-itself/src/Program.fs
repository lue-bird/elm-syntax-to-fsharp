module Program

[<EntryPoint>]
let main (_args: array<string>) : int =
    let elmJsonSource : string = (new System.IO.StreamReader "elm.json").ReadToEnd()
    match
        Elm.ElmProjectToFsharp_elmJsonToProjectAndDependencySourceDirectories
            (Elm.StringRopeOne elmJsonSource)
    with
    | Error error ->
        stderr.WriteLine (Elm.StringRope.toString error)
        1
    | Ok sourceDirectoriesToRead ->
        let moduleSources: seq<string> =
            Seq.collect
                (fun sourceDirectoryToRead ->
                    Seq.map 
                        (fun file -> System.IO.File.ReadAllText file)
                        (System.IO.Directory.EnumerateFiles(
                            Elm.StringRope.toString sourceDirectoryToRead,
                            "*.elm",
                            System.IO.SearchOption.AllDirectories
                        ))
                )
                sourceDirectoriesToRead
        match
            Elm.ElmProjectToFsharp_fromModuleSources
                (Seq.toList (Seq.map Elm.StringRopeOne moduleSources))
        with
        | Error error ->
            stderr.WriteLine (Elm.StringRope.toString error)
            1
        | Ok bundledFsharp ->
            System.IO.File.WriteAllText(
                "src/Elm.fs",
                Elm.StringRope.toString bundledFsharp
            )

            System.Console.WriteLine("Successfully wrote the bundled code into src/Elm.fs.")
            0
