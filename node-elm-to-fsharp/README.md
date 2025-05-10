Transpile all elm modules in the current project
(source-directories + dependencies)
into a bundled `src/Elm.fs` file that exposes every value/function declaration
(e.g. `Main_runOnString` for `Main.runOnString`)


```bash
npm install && npm run build
```

To instead run it once

```bash
npm run start
```

See also [how to use the transpiled output](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main#how-to-use-the-transpiled-output).
