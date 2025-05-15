#!/usr/bin/env node
import * as Node from "@lue-bird/elm-state-interface-experimental/node"
// import * as v8 from "node:v8"
import { Elm } from "./elmMain.js"

const elmApp = Elm.Main.init()
// ↓ workers not implemented, yet
// ↓ https://github.com/nodejs/node/issues/44014
// v8.startupSnapshot.setDeserializeMainFunction(() => {
Node.programStart({ ports: elmApp.ports })
// })
