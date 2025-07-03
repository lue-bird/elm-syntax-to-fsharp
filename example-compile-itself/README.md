More or less a drop-in replacement for [node-elm-to-fsharp](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/node-elm-to-fsharp).

There's no real point of this (beyond a proof of concept that it can compile itself) because
- the setup for users is more complicated (build node-elm-to-fsharp, install dotnet, wait a minute for `dotnet publish`)
- ahead of time compiling this project currently runs into a dotnet bug when running (`System.TypeLoadException`). Maybe some dontnet update will fix it
