This is actually the formatter I use on all my projects.
It's nice because its about 3 times faster than `elm-format` so I never have to wait for it.

After building it, you can set it for the elm language server with
```json
"elmLS.elmFormatPath": "[where you cloned this repo]/elm-syntax-to-fsharp/example-elm-formatter/bin/release/net9.0/[your-architecture]/native/example-elm-formatter"
```
