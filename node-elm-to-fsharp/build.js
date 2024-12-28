import * as elmEsm from "elm-esm"
import * as fs from "node:fs"
import * as elmOptimizeLevel2 from "elm-optimize-level-2"

const elmMainFilePath = "src/Main.elm"
const compiledElmMainJsFilePath = "src/elmMain.js"

await elmOptimizeLevel2.run({
    inputFilePath: [elmMainFilePath],
    outputFilePath: compiledElmMainJsFilePath,
    optimizeSpeed: true,
    processOpts: null
})
const optimizedCompiledCode = await fs.promises.readFile(compiledElmMainJsFilePath, { encoding: "utf-8" })

const optimizedCompiledCodeAsEsm = elmEsm.toESModule(optimizedCompiledCode)

await fs.promises.writeFile(compiledElmMainJsFilePath, optimizedCompiledCodeAsEsm, { encoding: "utf-8" })
