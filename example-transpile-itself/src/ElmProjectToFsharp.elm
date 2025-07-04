module ElmProjectToFsharp exposing (elmJsonToProjectAndDependencySourceDirectories, fromModuleSources)

import Elm.Package
import Elm.Parser
import Elm.Project
import Elm.Version
import ElmSyntaxToFsharp
import Json.Decode
import Json.Encode


fromModuleSources : List String -> Result String String
fromModuleSources originalSources =
    case
        originalSources
            |> listFoldlWhileOkFrom []
                (\originalModuleSource soFar ->
                    case originalModuleSource |> Elm.Parser.parseToFile of
                        Err _ ->
                            Err originalModuleSource

                        Ok syntaxModule ->
                            Ok (syntaxModule :: soFar)
                )
    of
        Err source ->
            Err
                ("source couldn't be parsed. Check for compiler errors. Here's the start of the module:\n"
                    ++ (source |> String.left 50)
                )

        Ok parsedModules ->
            let
                transpiledDeclarationsAndErrors =
                    parsedModules
                        |> ElmSyntaxToFsharp.modules
            in
            case transpiledDeclarationsAndErrors.errors of
                error0 :: error1Up ->
                    Err ((error0 :: error1Up) |> String.join "\n")

                [] ->
                    Ok
                        (transpiledDeclarationsAndErrors.declarations
                            |> ElmSyntaxToFsharp.fsharpDeclarationsToModuleString
                            |> -- TODO remove for general use
                               String.replace
                                "ListExtra_uniqueHelp<'a>"
                                "ListExtra_uniqueHelp<'a when 'a: equality>"
                            |> String.replace
                                "ListExtra_unique<'a>"
                                "ListExtra_unique<'a when 'a: equality>"
                        )


elmJsonToProjectAndDependencySourceDirectories :
    { homeDirectory : String }
    -> String
    -> Result String (List String)
elmJsonToProjectAndDependencySourceDirectories environment elmJsonSource =
    case elmJsonSource |> Json.Decode.decodeString Elm.Project.decoder of
        Err elmJsonReadError ->
            Err (Json.Decode.errorToString elmJsonReadError)

        Ok elmJson ->
            case elmJson of
                Elm.Project.Application application ->
                    Ok
                        (application.dirs
                            ++ ((application.depsDirect
                                    ++ application.depsIndirect
                                )
                                    |> List.map
                                        (\( dependencyName, dependencyVersion ) ->
                                            packageSourceDirectoryPath
                                                { homeDirectory = environment.homeDirectory
                                                , packageName = dependencyName |> Elm.Package.toString
                                                , packageVersion = dependencyVersion |> Elm.Version.toString
                                                }
                                        )
                               )
                        )

                Elm.Project.Package _ ->
                    Err
                        "You're in a package but elm-to-fsharp only works for applications."


packageSourceDirectoryPath :
    { homeDirectory : String, packageName : String, packageVersion : String }
    -> String
packageSourceDirectoryPath info =
    info.homeDirectory
        ++ "/.elm/0.19.1/packages/"
        ++ info.packageName
        ++ "/"
        ++ info.packageVersion
        ++ "/src"



--


listFoldlWhileOkFrom :
    okFolded
    -> (a -> okFolded -> Result err okFolded)
    -> List a
    -> Result err okFolded
listFoldlWhileOkFrom initialOkFolded reduceOnOk list =
    case list of
        [] ->
            Ok initialOkFolded

        head :: tail ->
            case initialOkFolded |> reduceOnOk head of
                Err error ->
                    Err error

                Ok okFoldedWithHead ->
                    listFoldlWhileOkFrom okFoldedWithHead reduceOnOk tail
