Print pure [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as [F#](https://fsharp.org/) code.

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
    let sample_plus2 (n: int64) =
        (+) n (List.sum [ 2L ])

    ..and some default declarations..
"""
```

To try it out, you can
run [this node script](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/node-elm-to-fsharp).

### be aware

-   only a subset of elm is currently supported. not supported:
    -   ports, glsl
    -   `elm/regex`, `elm/file`, `elm/bytes`, `elm/http`, `elm/random`, `elm/url`, `elm/json`, `elm/parser`, `elm/virtual-dom`,
        `elm/html`, `elm/svg`, `elm/browser`, `elm/time`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`, `elm-explorations/linear-algebra`
    -   `Platform`, `Platform.Cmd`, `Platform.Sub`, `Task`, `Process`
        (also, `Set` is supported but not optimized).
    -   extensible record types. For example, these declarations won't work (at let or module level):
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
        Incidentally, avoiding extensible record types
        also tends to improve your elm code because it's simpler and makes the compiler errors more concrete
-   no compile checks are performed before transpiling to F#
-   the resulting code might not be readable or even conventionally formatted and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-fsharp/issues/new) you notice <3

### why F#?
-   it runs decently fast (remember to enable AOT compilation) and can even compile further into Wasm or using [fable](https://fable.io/) into languages like rust 
-   it's pretty much a superset of elm which makes transpiling easy

### how do I use the transpiled output?
An example can be found in [`example/`](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/example).

Add a project config
```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net9.0</TargetFramework>
    <RootNamespace>yourProjectName</RootNamespace>
    <PublishAot>true</PublishAot>
    <OptimizationPreference>Speed</OptimizationPreference>
    <StripSymbols>true</StripSymbols>
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
    let output = Elm.yourModule_yourFunction yourInput
    0
```
Here's some special types you can expect:
  - elm `Basics.Int`s will be of type `int64`.
    You can create them explicitly by appending L to an int literal (`42L`)
    or simply using any `int` (F# implicitly converts them)
    and unwrap them to an `int` with `int yourInt64`
  - elm `String.String`s will be of type `Elm.StringRope`.
    You can create them with `Elm.StringRopeOne yourFsharpString`
    and unwrap them with `Elm.StringRope.toString yourTranspiledString`
  - elm records like `{ a : Float, b : Float }` will provided as
    constructed type aliases for each field combination: `Elm.Generated_a_b<float, float>`.
    While the type might look weird, values can be created and used like any regular record.

Compile the resulting F# to an executable:
```bash
dotnet publish --self-contained -c release
```
The built executable can now be found at `/bin/release/net9.0/yourArchitecture/native/yourProjectName`.

If something unexpected happened,
please [report an issue](https://github.com/lue-bird/elm-syntax-to-fsharp/issues/new).
