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
    let sample_plus2 (n: int) =
        (+) n (List.sum [ 2 ])

    ..and some default declarations..
"""
```

To try it out, you can
run [this node script](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/node-elm-to-fsharp).

### be aware

-   only a subset of elm is currently supported. not supported:
    -   `elm/regex`, `elm/file`, `elm/bytes`, `elm/http`, `elm/random`, `elm/url`, `elm/json`, `elm/parser`, `elm/virtual-dom`,
        `elm/html`, `elm/svg`, `elm/browser`, `elm/time`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`, `elm-explorations/linear-algebra`
    -   `Platform`, `Platform.Cmd`, `Platform.Sub`, `Task`, `Process`
    -   ports, glsl, the prefix operator functions `(>>)` and `(<<)`
    -   extensible record types. For example, these declarations won't work (at let or module level):
        ```elm
        -- inferred { r | name : name } -> name
        getName =
            .name
        
        -- inferred name -> { r | name : name } -> { r | name : name }
        setName new r =
            { r | name = new }
        
        -- even if used in explicit annotation
        getName : { r | name : name } -> name
        getName =
            .name
        
        -- even if the extensible record type is "fully constructed"
        type alias Named otherFields =
            { otherFields | name : String }
        
        getName : Named { email : Email } -> String
        getName =
            .name
        ```
        Allowed however are for example:
        ```elm
        type alias User =
            { name : String, email : Email }
        
        userGetName : User -> String
        userGetName =
            .name
        ```
        Incidentally, avoiding extensible record types
        also tends to improve your elm code because it's simpler and makes the compiler errors more concrete
    -   potential future candidates: `Basics.(<<)`, `Basics.(>>)`, `Basics.clamp`, `Basics.degrees`, `Basics.turns`,
        `Basics.radians`, `Basics.logBase`, `Basics.atan2`, `Basics.toPolar`, `Basics.fromPolar`, `Basics.never`, `List.map5`, `List.map4`, `Char.isAlpha`, `Char.isAlphaNum`, `Char.isOctDigit`, `Char.toLocaleLower`, `Char.toLocaleUpper`, `List.head`, `List.tail`, `List.unzip`, `Dict.update`, `Dict.merge`, `Dict.intersect`, `Bitwise`, `Set`, `Array`. Any help appreciated!
-   no checks are performed before transpiling to fsharp. So if you don't add a compile check of your elm input,
    you might e.g. get a running program that circumvents an elm opaque type
-   not much care has been put into making the resulting code readable or even conventionally formatted
    and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-fsharp/issues/new) you notice <3

### why F#?
-   it runs decently fast (remember to enable AOT compilation) and can even compile further into Wasm or using [fable](https://fable.io/) even into languages like rust 
-   it's pretty much a superset of elm which makes transpiling easy
