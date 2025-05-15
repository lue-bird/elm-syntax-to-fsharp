Print pure [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as [F#](https://fsharp.org/) code.
To try it out, you can
run [this script](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/node-elm-to-fsharp).

```elm
import Elm.Parser
import ElmSyntaxToFsharp

"""module Sample exposing (..)

plus2 : Int -> Int
plus2 n =
    n + ([ 2 ] |> List.sum)
"""
    |> Elm.Parser.parseToFile
    |> Result.mapError (\_ -> "failed to parse elm source code")
    |> Result.map
        (\syntaxModule ->
            [ syntaxModule ]
                |> ElmSyntaxToFsharp.modules
                |> .declarations
                |> ElmSyntaxToFsharp.fsharpDeclarationsToModuleString
        )
-->
Ok """namespace global
module Elm =
    let Sample_plus2 (n: int64) =
        (+) n (List.sum [ 2L ])

    ..and some default declarations..
"""
```

### be aware

-   not supported are
    -   ports that use non-json values like `port sendMessage : String -> Cmd msg`, glsl
    -   `elm/file`, `elm/http`, `elm/virtual-dom`, `elm/html`, `elm/svg`, `elm/browser`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`, `elm-explorations/linear-algebra`
    -   `Task`, `Process`, `Platform.Task`, `Platform.ProcessId`, `Platform.Router`, `Platform.sendToApp`, `Platform.sendToSelf`, `Random.generate`, `Time.now`, `Time.every`, `Time.here`, `Time.getZoneName`, `Bytes.getHostEndianness`
    -   extensible record types. For example, these declarations might not work (at let or module level):
        ```elm
        -- in aliased type or variant value
        type alias Named otherFields =
            { otherFields | name : String }
        
        -- in explicit annotation
        getName : { r | name : name } -> name
        getName =
            .name
        
        -- in un-annotated type
        -- Here inferred as name -> { r | name : name } -> { r | name : name }
        setName new r =
            { r | name = new }
        ```
        Allowed however are for example:
        ```elm
        userGetName : { name : String, email : Email } -> String
        userGetName =
            .name
        ```
        In these cases we assume that you intended to use a regular record type with only the extension fields which can lead to F# compile errors if you actually pass in additional fields.

        Incidentally, avoiding extensible record types
        also tends to improve your elm code because it's simpler and makes the compiler errors more concrete
-   dependencies cannot internally use the same module names as the transpiled project
-   no compile checks are performed before transpiling to F#
-   the resulting code might not be readable or even conventionally formatted and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-fsharp/issues/new) you notice <3

### why F#?
-   it runs decently fast (remember to enable AOT compilation) and can even compile further into Wasm or using [fable](https://fable.io/) into languages like rust 
-   it's pretty much a superset of elm which makes transpiling easy

### how do I use the transpiled output?
An example can be found in [`example/`](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/example).

Add a project config `yourProjectName.fsproj`
```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net9.0</TargetFramework>
    <RootNamespace>yourProjectName</RootNamespace>
    <PublishAot>true</PublishAot>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="src/Elm.fs" />
    <Compile Include="src/Program.fs" />
  </ItemGroup>
</Project>
```
Add a file `Program.fs` that uses `Elm.fs`
```fs
module Program

[<EntryPoint>]
let main args =
    let output = Elm.YourModule_yourFunction yourInput
    0
```
where `Elm.YourModule_yourFunction` is the transpiled elm function `Your.Module.yourFunction`.

Here's some special types you can expect:
  - elm `Basics.Int`s will be of type `int64`.
    You can create them explicitly by appending L to an int literal (`42L`)
    or simply using any `int` (F# implicitly converts them)
    and unwrap them to an `int` with `int yourInt64`
  - elm `String.String`s will be of type `Elm.StringRope`.
    You can create them with `Elm.StringRopeOne yourFsharpString`
    and unwrap them with `Elm.StringRope.toString yourTranspiledString`
  - elm records like `{ a : Float, b : Float }` will be provided as
    constructed type aliases for each field combination: `Elm.Generated_A_B<float, float>`.
    While the type might look weird, values can be created and used like any regular record with uppercase field names, e.g. `{ A = 1.1; B = 2.2 }`
  - elm tuples/triples like `( float, float )`
    will be of type `(struct( float * float ))`.
    You can create them with `struct( 1.1, 2.2 )` und destructure them with `struct( a, b )`
  - elm `Json.Encode.Value`/`Json.Decode.Value` will be of type
    [`System.Text.Json.Nodes.JsonNode`](https://learn.microsoft.com/en-us/dotnet/api/system.text.json.nodes.jsonnode?view=net-9.0).
    Encode and decode them like you would in elm, like `Elm.JsonEncode_float 2.2`
  - elm `Regex` will be of type [`System.Text.RegularExpressions.Regex`](https://learn.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-9.0).
    Create them like you would in elm with `Elm.Regex_fromString`, `Elm.Regex_fromStringWith` or `Elm.Regex_never`
  - a transpiled elm app does not run itself.
    An elm main `Platform.worker` program type will literally just consist of fields `Init`, `Update` and `Subscriptions` where
    subscriptions/commands are returned as a list of `Elm.PlatformSub_SubSingle`/`Elm.PlatformCmd_CmdSingle` with possible elm subscriptions/commands in a choice type.
    It's then your responsibility as "the platform" to perform effects, create events and manage the state. For an example see [example-worker/](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/example-worker)

The rest is pretty obvious: `Float` → `float`, `Char` → `char`, `Bool` → `bool`, `()` → `unit` (create and match with `()`), `List Float` -> `List<float>`, `Array Float` → `array<float>`, `Set Float` -> `Set<float>`, `Dict Float Float` → `Map<float, float>`. `Maybe Float` → `option<float>`, `Result error value` → `Result<'value, 'error>`, `Order` → `Elm.Basics_Order` (enum).

Compile the resulting F# to an executable:
```bash
dotnet publish --self-contained -c release
```
The built executable can now be found at `/bin/release/net9.0/yourArchitecture/native/yourProjectName`.

If something unexpected happened,
please [report an issue](https://github.com/lue-bird/elm-syntax-to-fsharp/issues/new).
