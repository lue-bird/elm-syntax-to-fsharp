// turn DefaultDeclarations.fs into the escaped elm string
// to be inserted at ElmSyntaxToFsharp.defaultDeclarations

import * as fs from "node:fs"
import * as path from "node:path"


const defaultDeclarationsFsharpFile =
    fs.readFileSync(
        path.join(import.meta.dirname, "DefaultDeclarations.fs"),
        { encoding: "utf-8" }
    )
const elmString =
    "\"\"\""
    + defaultDeclarationsFsharpFile
        .replaceAll(
            `namespace global

module Elm =`,
            ""
        ).replaceAll("\\", "\\\\")
    + "\"\"\""
fs.writeFileSync(
    path.join(import.meta.dirname, "DefaultDeclarations.elm"),
    elmString,
    { encoding: "utf-8" }
)
