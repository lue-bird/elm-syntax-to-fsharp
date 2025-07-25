Print [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as [F#](https://fsharp.org/) code.
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
Ok """module Elm
let Sample_plus2 (n: int64) : int64 =
    (+) n (List.sum [ 2L ])

..and some default declarations..
"""
```

### be aware

-   not supported are
    -   ports that use non-json values like `port sendMessage : String -> Cmd msg`, glsl
    -   `elm/file`, `elm/http`, `elm/browser`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`
    -   `Task`, `Process`, `Platform.Task`, `Platform.ProcessId`, `Platform.Router`, `Platform.sendToApp`, `Platform.sendToSelf`, `Random.generate`, `Time.now`, `Time.every`, `Time.here`, `Time.getZoneName`, `Bytes.getHostEndianness`, `Math.Matrix4.inverseOrthonormal`, `Math.Matrix4.mulAffine`
    -   extensible record types outside of module-level value/function declarations. For example, these declarations might not work:
        ```elm
        -- in variant value
        type Named rec = Named { rec | name : String }
        -- in let type, annotated or not
        let getName : { r | name : name } -> name
        ```
        Allowed is only record extension in module-level value/functions, annotated or not:
        ```elm
        userId : { u | name : String, server : Domain } -> String
        ```
        In the non-allowed cases listed above, we assume that you intended to use a regular record type with only the extension fields which can lead to F# compile errors if you actually pass in additional fields.
-   elm-exploration/linear-algebra's `Vec2`, `Vec3`, `Vec4`, `Mat4` components have 64-bit precision but their F# counterparts only have 32
-   dependencies cannot internally use the same module names as the transpiled project
-   the resulting code might not be readable or even conventionally formatted and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-fsharp/issues/new) you notice <3

### why F#?
-   it runs decently fast (remember to enable AOT compilation) and can even compile further into Wasm or using [fable](https://fable.io/) into languages like rust 
-   it's pretty much a superset of elm which makes transpiling easy

### how do I use the transpiled output?
An example can be found in [`example-hello-world/`](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/example-hello-world).

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
Add a file `src/Program.fs` that uses `src/Elm.fs`
```fsharp
module Program

[<EntryPoint>]
let main args =
    let output = Elm.YourModule_yourFunction yourInput
    0
```
where `Elm.YourModule_yourFunction` is the transpiled elm function `Your.Module.yourFunction`. (If the value/function contains `number` type variables or extensible records, search for `Elm.YourModule_yourFunction__` to see the different specialized options)

You will find these types:
  - elm `Float` → F# `float`, `Bool` → `bool`, `()` → `unit` (create and match with `()`), `List Float` -> `List<float>`, `Array Float` → `array<float>`, `Set Float` -> `Set<float>`, `Dict Float Char` → `Map<float, char>`, `Result Float Char` → `Result<char, float>`, `Order` → `Elm.Basics_Order` (enum)
  - elm `Int`s will be of type `int64`.
    You can create them explicitly by appending L to an int literal (`42L`)
    or simply using any `int` (F# implicitly converts them)
    and unwrap them to an `int` with `int yourInt64`
  - elm `String`s will be of type `Elm.StringRope`.
    You can create them with `Elm.StringRope.fromString yourFsharpString`
    and unwrap them with `Elm.StringRope.toString yourTranspiledString`
  - elm `Char`s will be of type `int`.
    You can create them with e.g. `int 'a'` or the char code.
    They represent the [value of a unicode scalar/"rune"](https://learn.microsoft.com/en-us/dotnet/api/system.text.rune.value?view=net-9.0) which is comprised of 1-2 UTF-16 `char`s
  - elm records like `{ a : Float, b : Float }` will be provided as
    constructed type aliases for each field combination: `Elm.Generated_A_B<float, float>`.
    While the type might look weird, values can be created and used like any regular record with uppercase field names, e.g. `{ A = 1.1; B = 2.2 }`
  - elm tuples/triples like `( Float, Float )`
    will be of type `(struct( float * float ))`.
    You can create them with `struct( 1.1, 2.2 )` und destructure them with `struct( a, b )`
  - elm `Maybe.Maybe`s will be of type `ValueOption`.
    You can create and match them with `ValueSome` for `Just` and `ValueNone` for `Nothing`
  - elm `Json.Encode.Value`/`Json.Decode.Value` will be of type
    [`System.Text.Json.Nodes.JsonNode`](https://learn.microsoft.com/en-us/dotnet/api/system.text.json.nodes.jsonnode?view=net-9.0).
    Encode and decode them like you would in elm, like `Elm.JsonEncode_float 2.2`
  - a transpiled elm app does not run itself.
    An elm main `Platform.worker` program type will literally just consist of fields `Init`, `Update` and `Subscriptions` where
    subscriptions/commands are returned as a list of `Elm.PlatformSub_SubSingle`/`Elm.PlatformCmd_CmdSingle` with possible elm subscriptions/commands in a choice type.
    It's then your responsibility as "the platform" to perform effects, create events and manage the state. For an example see [example-worker/](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/example-worker)
  - elm `Regex` will be of type [`System.Text.RegularExpressions.Regex`](https://learn.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-9.0).
    Create them like you would in elm with `Elm.Regex_fromString`, `Elm.Regex_fromStringWith` or `Elm.Regex_never`
  - elm-exploration/linear-algebra's `Math.Vector2.Vec2`, `Math.Vector3.Vec3`, `Math.Vector4.Vec4`, `Math.Matrix4.Mat4` will be of type [`System.Numerics.Vector2`](https://learn.microsoft.com/en-us/dotnet/api/system.numerics.vector2?view=net-9.0), [`System.Numerics.Vector3`](https://learn.microsoft.com/en-us/dotnet/api/system.numerics.vector3?view=net-9.0), [`System.Numerics.Vector4`](https://learn.microsoft.com/en-us/dotnet/api/system.numerics.vector4?view=net-9.0), [`System.Numerics.Matrix4x4`](https://learn.microsoft.com/en-us/dotnet/api/system.numerics.matrix4x4?view=net-9.0)

Compile the resulting F# to an executable:
```bash
dotnet publish --self-contained -c release
```
The built executable can now be found at `/bin/release/net9.0/yourArchitecture/native/yourProjectName`.

Or build and run it once:
```bash
dotnet run
```
append ` --no-restore` on consecutive runs to make it faster.

If something unexpected happened,
please [report an issue](https://github.com/lue-bird/elm-syntax-to-fsharp/issues/new).
