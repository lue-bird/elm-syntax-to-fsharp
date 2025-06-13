namespace client

open Bolero.Remoting.Client

module Program =

    [<EntryPoint>]
    let Main (args: array<string>) : int =
        let builder =
            Microsoft.AspNetCore.Components.WebAssembly.Hosting.WebAssemblyHostBuilder.CreateDefault
                args

        builder.Services.AddBoleroRemoting builder.HostEnvironment |> ignore
        builder.Build().RunAsync() |> ignore
        0
