Run with
```bash
./../../node-elm-to-fsharp/dist/elm-to-fsharp && dotnet run --project server
```
after [building the transpile script](https://github.com/lue-bird/elm-syntax-to-fsharp/tree/main/node-elm-to-fsharp)

> If it throws an error about "configured user limit (128) on the number of inotify instances", try
> ```bash
> echo fs.inotify.max_user_instances=524288 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p
> ```
> See issues like https://github.com/dotnet/aspnetcore/issues/8449

Also, sorry for this insanely bloated hello world;
I couldn't find a simpler starting point.
If you found something simpler, please [tell me about it](https://github.com/lue-bird/elm-syntax-to-fsharp/issues/new), thanks.
Preferably the program would just spit out html,js,wasm etc in a folder that any external server can serve.
