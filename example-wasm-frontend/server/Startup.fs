module server.Program


open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Bolero.Remoting.Server
open Bolero.Server
open Bolero.Templating.Server

[<Microsoft.AspNetCore.Components.Route "/{*path}">]
type IndexPage() =
    inherit Bolero.Component()

    override _.Render() =
        Bolero.Server.Html.doctypeHtml {
            Bolero.Html.head {
                Bolero.Html.meta { Bolero.Html.attr.charset "UTF-8" }

                Bolero.Html.meta {
                    Bolero.Html.attr.name "viewport"
                    Bolero.Html.attr.content "width=device-width, initial-scale=1.0"
                }

                Bolero.Html.title { "example wasm frontend" }
            }

            Bolero.Html.body {
                Bolero.Html.div {
                    Bolero.Html.attr.id "main"

                    Bolero.Html.comp<client.Main.App> {
                        Bolero.Server.Html.attr.renderMode
                            Microsoft.AspNetCore.Components.Web.RenderMode.InteractiveWebAssembly
                    }
                }

                Bolero.Server.Html.boleroScript
            }
        }

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services
        .AddRazorComponents()
        .AddInteractiveServerComponents()
        .AddInteractiveWebAssemblyComponents()
    |> ignore

    builder.Services.AddServerSideBlazor() |> ignore

    builder.Services.AddBoleroComponents() |> ignore
#if DEBUG
    builder.Services.AddHotReload(templateDir = __SOURCE_DIRECTORY__ + "/../client")
    |> ignore
#endif

    let app = builder.Build()

    if app.Environment.IsDevelopment() then
        app.UseWebAssemblyDebugging()

    app.UseStaticFiles().UseRouting().UseAuthorization().UseAntiforgery() |> ignore

#if DEBUG
    app.UseHotReload()
#endif
    app.MapBoleroRemoting() |> ignore

    app
        .MapRazorComponents<IndexPage>()
        .AddInteractiveServerRenderMode()
        .AddInteractiveWebAssemblyRenderMode()
        .AddAdditionalAssemblies(typeof<client.Main.App>.Assembly)
    |> ignore

    app.Run()

    0
