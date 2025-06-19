namespace global

open Microsoft.Extensions.DependencyInjection


module Program =
    [<EntryPoint>]
    let Main args =
        let builder =
            Microsoft.AspNetCore.Components.WebAssembly.Hosting.WebAssemblyHostBuilder.CreateDefault
                args

        builder.RootComponents.Add<Main.App> "#main"

        builder.Services.AddScoped<System.Net.Http.HttpClient>(fun _ ->
            new System.Net.Http.HttpClient(
                BaseAddress = System.Uri builder.HostEnvironment.BaseAddress
            ))
        |> ignore

        builder.Build().RunAsync() |> ignore
        0
