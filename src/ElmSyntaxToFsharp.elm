module ElmSyntaxToFsharp exposing
    ( modules, fsharpDeclarationsToModuleString
    , FsharpLetDeclaration(..), FsharpExpression(..), FsharpPattern(..), FsharpType(..)
    )

{-| Transpiling [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
declarations to fsharp.

@docs modules, fsharpDeclarationsToModuleString
@docs FsharpLetDeclaration, FsharpExpression, FsharpPattern, FsharpType

If you need more fine-grained helpers,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

import Bitwise
import Data.Graph
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import ElmSyntaxTypeInfer
import FastDict
import FastSet
import Print exposing (Print)
import Unicode


{-| The sub-set of fsharp type syntax used in generated code
-}
type FsharpType
    = FsharpTypeConstruct
        { moduleOrigin : Maybe String
        , name : String
        , arguments : List FsharpType
        }
    | FsharpTypeTuple
        { part0 : FsharpType
        , part1 : FsharpType
        , part2Up : List FsharpType
        }
    | FsharpTypeVariable String
    | FsharpTypeFunction
        { input : FsharpType
        , output : FsharpType
        }


{-| The sub-set of fsharp pattern syntax used in generated code
-}
type FsharpPattern
    = FsharpPatternIgnore
    | FsharpPatternInt Int
    | FsharpPatternChar Char
    | FsharpPatternString String
    | FsharpPatternVariable String
    | FsharpPatternAs
        { variable : String
        , pattern : FsharpPattern
        }
    | FsharpPatternListCons
        { head : FsharpPattern
        , tail : FsharpPattern
        }
    | FsharpPatternListExact (List FsharpPattern)
    | FsharpPatternRecordInexhaustive (FastDict.Dict String FsharpPattern)
    | FsharpPatternVariant
        { moduleOrigin : Maybe String
        , name : String
        , values : List FsharpPattern
        }
    | FsharpPatternTuple
        { part0 : FsharpPattern
        , part1 : FsharpPattern
        , part2Up : List FsharpPattern
        }


{-| The sub-set of fsharp expression syntax used in generated code
-}
type FsharpExpression
    = FsharpExpressionUnit
    | FsharpExpressionFloat Float
    | FsharpExpressionInt Int
    | FsharpExpressionChar Char
    | FsharpExpressionString String
    | FsharpExpressionReference
        { moduleOrigin : Maybe String
        , name : String
        }
    | FsharpExpressionRecordAccess
        { record : FsharpExpression
        , field : String
        }
    | FsharpExpressionTuple
        { part0 : FsharpExpression
        , part1 : FsharpExpression
        , part2Up : List FsharpExpression
        }
    | FsharpExpressionIfElse
        { condition : FsharpExpression
        , onTrue : FsharpExpression
        , onFalse : FsharpExpression
        }
    | FsharpExpressionList (List FsharpExpression)
    | FsharpExpressionRecord (FastDict.Dict String FsharpExpression)
    | FsharpExpressionRecordUpdate
        { originalRecordVariable : String
        , fields : FastDict.Dict String FsharpExpression
        }
    | FsharpExpressionCall
        { called : FsharpExpression
        , arguments : List FsharpExpression
        }
    | FsharpExpressionLambda
        { parameters : List { pattern : FsharpPattern, type_ : FsharpType }
        , result : FsharpExpression
        }
    | FsharpExpressionMatchWith
        { matched : FsharpExpression
        , case0 :
            { pattern : FsharpPattern
            , patternType : FsharpType
            , result : FsharpExpression
            }
        , case1Up :
            List
                { pattern : FsharpPattern
                , patternType : FsharpType
                , result : FsharpExpression
                }
        }
    | FsharpExpressionWithLetDeclarations
        { declaration0 : FsharpLetDeclaration
        , declaration1Up : List FsharpLetDeclaration
        , result : FsharpExpression
        }


{-| The sub-set of fsharp local declaration syntax used in generated fsharp code
-}
type FsharpLetDeclaration
    = FsharpLetDestructuring
        { pattern : FsharpPattern
        , patternType : FsharpType
        , expression : FsharpExpression
        }
    | FsharpLetDeclarationValueOrFunction
        { name : String
        , result : FsharpExpression
        , type_ : FsharpType
        }


{-| How do references used in a module map to their origin module?

Contains variants, variant function and value declaration names.

-}
type alias ModuleContext =
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , recordTypeAliasLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            (List String)
    , portIncomingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , portOutgoingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    }


{-| Calculate valid mappings of qualifications + name
to origin module based on a module's imports.

Requires all exposed names
so we can resolve `exposing (..)` and `ChoiceType(..)`.

-}
importsToModuleContext :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { valueOrFunctionOrTypeAliasNames : FastSet.Set String
        , choiceTypesExposingVariants :
            FastDict.Dict String (FastDict.Dict String { valueCount : Int })
        , recordTypeAliases :
            FastDict.Dict String (List String)
        , portsIncoming : FastSet.Set String
        , portsOutgoing : FastSet.Set String
        }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> ModuleContext
importsToModuleContext moduleExposes imports =
    let
        importsNormal :
            List
                { moduleName : Elm.Syntax.ModuleName.ModuleName
                , alias : Maybe String
                , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                    FastSet.Set String
                , exposedVariants :
                    FastDict.Dict String { valueCount : Int }
                , exposedRecordTypeAliases :
                    FastDict.Dict String (List String)
                , exposedPortsOutgoing : FastSet.Set String
                , exposedPortsIncoming : FastSet.Set String
                }
        importsNormal =
            implicitImports
                ++ (imports
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ syntaxImport) ->
                                let
                                    importModuleName : Elm.Syntax.ModuleName.ModuleName
                                    importModuleName =
                                        syntaxImport.moduleName |> Elm.Syntax.Node.value

                                    exposes :
                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                                            FastSet.Set String
                                        , variants :
                                            FastDict.Dict String { valueCount : Int }
                                        , recordTypeAliases :
                                            FastDict.Dict String (List String)
                                        , portsOutgoing : FastSet.Set String
                                        , portsIncoming : FastSet.Set String
                                        }
                                    exposes =
                                        case syntaxImport.exposingList of
                                            Nothing ->
                                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                    FastSet.empty
                                                , variants = FastDict.empty
                                                , recordTypeAliases = FastDict.empty
                                                , portsOutgoing = FastSet.empty
                                                , portsIncoming = FastSet.empty
                                                }

                                            Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                                case moduleExposes |> FastDict.get importModuleName of
                                                    Nothing ->
                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                            FastSet.empty
                                                        , variants = FastDict.empty
                                                        , recordTypeAliases = FastDict.empty
                                                        , portsOutgoing = FastSet.empty
                                                        , portsIncoming = FastSet.empty
                                                        }

                                                    Just moduleExposedNames ->
                                                        case syntaxExposing of
                                                            Elm.Syntax.Exposing.All _ ->
                                                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                    moduleExposedNames.choiceTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\choiceTypeName _ soFar ->
                                                                                soFar |> FastSet.insert choiceTypeName
                                                                            )
                                                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                                                , variants =
                                                                    moduleExposedNames.choiceTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\_ variantNames soFar -> FastDict.union variantNames soFar)
                                                                            FastDict.empty
                                                                , recordTypeAliases =
                                                                    moduleExposedNames.recordTypeAliases
                                                                , portsOutgoing = moduleExposedNames.portsOutgoing
                                                                , portsIncoming = moduleExposedNames.portsIncoming
                                                                }

                                                            Elm.Syntax.Exposing.Explicit explicitEposes ->
                                                                explicitEposes
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ expose) soFar ->
                                                                            case expose of
                                                                                Elm.Syntax.Exposing.InfixExpose _ ->
                                                                                    soFar

                                                                                Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    , recordTypeAliases =
                                                                                        case moduleExposedNames.recordTypeAliases |> FastDict.get name of
                                                                                            Nothing ->
                                                                                                soFar.recordTypeAliases

                                                                                            Just fieldOrder ->
                                                                                                soFar.recordTypeAliases
                                                                                                    |> FastDict.insert name fieldOrder
                                                                                    , portsOutgoing = soFar.portsOutgoing
                                                                                    , portsIncoming = soFar.portsIncoming
                                                                                    }

                                                                                Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                    if moduleExposedNames.portsOutgoing |> FastSet.member name then
                                                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                            soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                        , variants = soFar.variants
                                                                                        , recordTypeAliases = soFar.recordTypeAliases
                                                                                        , portsOutgoing =
                                                                                            soFar.portsOutgoing
                                                                                                |> FastSet.insert name
                                                                                        , portsIncoming = soFar.portsIncoming
                                                                                        }

                                                                                    else if moduleExposedNames.portsIncoming |> FastSet.member name then
                                                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                            soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                        , variants = soFar.variants
                                                                                        , recordTypeAliases = soFar.recordTypeAliases
                                                                                        , portsOutgoing = soFar.portsOutgoing
                                                                                        , portsIncoming =
                                                                                            soFar.portsIncoming
                                                                                                |> FastSet.insert name
                                                                                        }

                                                                                    else
                                                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                            soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                                |> FastSet.insert name
                                                                                        , variants = soFar.variants
                                                                                        , recordTypeAliases = soFar.recordTypeAliases
                                                                                        , portsOutgoing = soFar.portsOutgoing
                                                                                        , portsIncoming = soFar.portsIncoming
                                                                                        }

                                                                                Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                            |> FastSet.insert choiceTypeExpose.name
                                                                                    , variants =
                                                                                        case choiceTypeExpose.open of
                                                                                            Nothing ->
                                                                                                soFar.variants

                                                                                            Just _ ->
                                                                                                case
                                                                                                    moduleExposedNames.choiceTypesExposingVariants
                                                                                                        |> FastDict.get choiceTypeExpose.name
                                                                                                of
                                                                                                    Nothing ->
                                                                                                        soFar.variants

                                                                                                    Just choiceTypeDeclared ->
                                                                                                        FastDict.union
                                                                                                            soFar.variants
                                                                                                            choiceTypeDeclared
                                                                                    , recordTypeAliases = soFar.recordTypeAliases
                                                                                    , portsOutgoing = soFar.portsOutgoing
                                                                                    , portsIncoming = soFar.portsIncoming
                                                                                    }
                                                                        )
                                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                            FastSet.empty
                                                                        , variants = FastDict.empty
                                                                        , recordTypeAliases = FastDict.empty
                                                                        , portsOutgoing = FastSet.empty
                                                                        , portsIncoming = FastSet.empty
                                                                        }
                                in
                                { moduleName = importModuleName
                                , alias =
                                    syntaxImport.moduleAlias
                                        |> Maybe.map
                                            (\(Elm.Syntax.Node.Node _ syntaxAlias) ->
                                                syntaxAlias |> String.join "."
                                            )
                                , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                    exposes.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                , exposedVariants =
                                    exposes.variants
                                , exposedRecordTypeAliases =
                                    exposes.recordTypeAliases
                                , exposedPortsOutgoing = exposes.portsOutgoing
                                , exposedPortsIncoming = exposes.portsIncoming
                                }
                            )
                   )
                |> importsCombine
    in
    importsNormal
        |> List.foldl
            (\syntaxImport soFar ->
                let
                    importedModuleMembers :
                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                            FastSet.Set String
                        , variants : FastDict.Dict String { valueCount : Int }
                        , recordTypeAliases :
                            FastDict.Dict String (List String)
                        , portsIncoming : FastSet.Set String
                        , portsOutgoing : FastSet.Set String
                        }
                    importedModuleMembers =
                        case moduleExposes |> FastDict.get syntaxImport.moduleName of
                            Nothing ->
                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                    FastSet.empty
                                , variants = FastDict.empty
                                , recordTypeAliases = FastDict.empty
                                , portsIncoming = FastSet.empty
                                , portsOutgoing = FastSet.empty
                                }

                            Just moduleExposedNames ->
                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                    moduleExposedNames.choiceTypesExposingVariants
                                        |> FastDict.foldl
                                            (\choiceTypeName _ namesSoFar ->
                                                namesSoFar
                                                    |> FastSet.insert choiceTypeName
                                            )
                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                , variants =
                                    moduleExposedNames.choiceTypesExposingVariants
                                        |> FastDict.foldl
                                            (\_ variantNames variantsSoFar ->
                                                FastDict.union variantNames variantsSoFar
                                            )
                                            FastDict.empty
                                , recordTypeAliases =
                                    moduleExposedNames.recordTypeAliases
                                , portsIncoming = moduleExposedNames.portsIncoming
                                , portsOutgoing = moduleExposedNames.portsOutgoing
                                }
                in
                moduleContextMerge
                    (moduleContextMerge
                        { variantLookup =
                            syntaxImport.exposedVariants
                                |> FastDict.foldl
                                    (\variantName variantInfo dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], variantName )
                                                { moduleOrigin = syntaxImport.moduleName
                                                , valueCount = variantInfo.valueCount
                                                }
                                    )
                                    FastDict.empty
                        , valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            syntaxImport.exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                |> FastSet.foldl
                                    (\expose dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], expose )
                                                syntaxImport.moduleName
                                    )
                                    FastDict.empty
                        , recordTypeAliasLookup =
                            syntaxImport.exposedRecordTypeAliases
                                |> FastDict.foldl
                                    (\typeAliasName fieldOrder dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], typeAliasName )
                                                fieldOrder
                                    )
                                    FastDict.empty
                        , portIncomingLookup =
                            syntaxImport.exposedPortsIncoming
                                |> FastSet.map (\portName -> ( [], portName ))
                        , portOutgoingLookup =
                            syntaxImport.exposedPortsOutgoing
                                |> FastSet.map (\portName -> ( [], portName ))
                        }
                        (case syntaxImport.alias of
                            Nothing ->
                                { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                    importedModuleMembers.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                , recordTypeAliasLookup =
                                    syntaxImport.exposedRecordTypeAliases
                                        |> FastDict.foldl
                                            (\typeAliasName fieldOrder dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert ( syntaxImport.moduleName, typeAliasName )
                                                        fieldOrder
                                            )
                                            FastDict.empty
                                , portIncomingLookup =
                                    syntaxImport.exposedPortsIncoming
                                        |> FastSet.map (\portName -> ( syntaxImport.moduleName, portName ))
                                , portOutgoingLookup =
                                    syntaxImport.exposedPortsOutgoing
                                        |> FastSet.map (\portName -> ( syntaxImport.moduleName, portName ))
                                }

                            Just importAlias ->
                                { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                    importedModuleMembers.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                , recordTypeAliasLookup =
                                    syntaxImport.exposedRecordTypeAliases
                                        |> FastDict.foldl
                                            (\typeAliasName fieldOrder dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert ( [ importAlias ], typeAliasName )
                                                        fieldOrder
                                            )
                                            FastDict.empty
                                , portIncomingLookup =
                                    syntaxImport.exposedPortsIncoming
                                        |> FastSet.map (\portName -> ( [ importAlias ], portName ))
                                , portOutgoingLookup =
                                    syntaxImport.exposedPortsOutgoing
                                        |> FastSet.map (\portName -> ( [ importAlias ], portName ))
                                }
                        )
                    )
                    soFar
            )
            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                FastDict.empty
            , variantLookup = FastDict.empty
            , recordTypeAliasLookup = FastDict.empty
            , portIncomingLookup = FastSet.empty
            , portOutgoingLookup = FastSet.empty
            }


moduleContextMerge : ModuleContext -> ModuleContext -> ModuleContext
moduleContextMerge a b =
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
        FastDict.union
            a.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
            b.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
    , variantLookup =
        FastDict.union
            a.variantLookup
            b.variantLookup
    , recordTypeAliasLookup =
        FastDict.union
            a.recordTypeAliasLookup
            b.recordTypeAliasLookup
    , portIncomingLookup =
        FastSet.union a.portIncomingLookup b.portIncomingLookup
    , portOutgoingLookup =
        FastSet.union a.portOutgoingLookup b.portOutgoingLookup
    }


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        , exposedRecordTypeAliases :
            FastDict.Dict String (List String)
        , exposedPortsOutgoing : FastSet.Set String
        , exposedPortsIncoming : FastSet.Set String
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants =
            FastDict.fromList
                [ ( "EQ", { valueCount = 0 } )
                , ( "LT", { valueCount = 0 } )
                , ( "GT", { valueCount = 0 } )
                , ( "True", { valueCount = 0 } )
                , ( "False", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList
                [ "Int"
                , "Float"
                , "toFloat"
                , "round"
                , "floor"
                , "ceiling"
                , "truncate"
                , "max"
                , "min"
                , "compare"
                , "Order"
                , "Bool"
                , "not"
                , "xor"
                , "modBy"
                , "remainderBy"
                , "negate"
                , "abs"
                , "clamp"
                , "sqrt"
                , "logBase"
                , "e"
                , "pi"
                , "cos"
                , "sin"
                , "tan"
                , "acos"
                , "asin"
                , "atan"
                , "atan2"
                , "degrees"
                , "radians"
                , "turns"
                , "toPolar"
                , "fromPolar"
                , "isNaN"
                , "isInfinite"
                , "identity"
                , "always"
                , "Never"
                , "never"
                ]
      }
    , { moduleName = [ "List" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "List" ]
      }
    , { moduleName = [ "Maybe" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants =
            FastDict.fromList
                [ ( "Just", { valueCount = 1 } )
                , ( "Nothing", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Maybe" ]
      }
    , { moduleName = [ "Result" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants =
            FastDict.fromList
                [ ( "Ok", { valueCount = 1 } )
                , ( "Err", { valueCount = 1 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Result" ]
      }
    , { moduleName = [ "String" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "String" ]
      }
    , { moduleName = [ "Char" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Char" ]
      }
    , { moduleName = [ "Tuple" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.empty
      }
    , { moduleName = [ "Debug" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.empty
      }
    , { moduleName = [ "Platform" ]
      , alias = Nothing
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Program" ]
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , alias = Just "Cmd"
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Cmd" ]
      }
    , { moduleName = [ "Platform", "Sub" ]
      , alias = Just "Sub"
      , exposedRecordTypeAliases = FastDict.empty
      , exposedPortsOutgoing = FastSet.empty
      , exposedPortsIncoming = FastSet.empty
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Sub" ]
      }
    ]


importsCombine :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        , exposedRecordTypeAliases :
            FastDict.Dict String (List String)
        , exposedPortsOutgoing : FastSet.Set String
        , exposedPortsIncoming : FastSet.Set String
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            , exposedRecordTypeAliases :
                FastDict.Dict String (List String)
            , exposedPortsOutgoing : FastSet.Set String
            , exposedPortsIncoming : FastSet.Set String
            }
importsCombine syntaxImports =
    importsCombineFrom [] syntaxImports


importsCombineFrom :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        , exposedRecordTypeAliases :
            FastDict.Dict String (List String)
        , exposedPortsOutgoing : FastSet.Set String
        , exposedPortsIncoming : FastSet.Set String
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            , exposedRecordTypeAliases :
                FastDict.Dict String (List String)
            , exposedPortsOutgoing : FastSet.Set String
            , exposedPortsIncoming : FastSet.Set String
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            , exposedRecordTypeAliases :
                FastDict.Dict String (List String)
            , exposedPortsOutgoing : FastSet.Set String
            , exposedPortsIncoming : FastSet.Set String
            }
importsCombineFrom soFar syntaxImports =
    case syntaxImports of
        [] ->
            soFar

        [ onlyImport ] ->
            onlyImport :: soFar

        import0 :: import1 :: import2Up ->
            if import0.moduleName == import1.moduleName then
                importsCombineFrom soFar
                    (importsMerge import0 import1
                        :: import2Up
                    )

            else
                importsCombineFrom
                    (import0 :: soFar)
                    (import1 :: import2Up)


importsMerge :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
        FastSet.Set String
    , exposedVariants :
        FastDict.Dict String { valueCount : Int }
    , exposedRecordTypeAliases :
        FastDict.Dict String (List String)
    , exposedPortsOutgoing : FastSet.Set String
    , exposedPortsIncoming : FastSet.Set String
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        , exposedRecordTypeAliases :
            FastDict.Dict String (List String)
        , exposedPortsOutgoing : FastSet.Set String
        , exposedPortsIncoming : FastSet.Set String
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        , exposedRecordTypeAliases :
            FastDict.Dict String (List String)
        , exposedPortsOutgoing : FastSet.Set String
        , exposedPortsIncoming : FastSet.Set String
        }
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , alias =
        case earlier.alias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.alias
    , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
        FastSet.union
            earlier.exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes
            later.exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes
    , exposedVariants =
        FastDict.union
            earlier.exposedVariants
            later.exposedVariants
    , exposedRecordTypeAliases =
        FastDict.union
            earlier.exposedRecordTypeAliases
            later.exposedRecordTypeAliases
    , exposedPortsOutgoing =
        FastSet.union earlier.exposedPortsIncoming
            later.exposedPortsIncoming
    , exposedPortsIncoming =
        FastSet.union earlier.exposedPortsOutgoing
            later.exposedPortsOutgoing
    }


fsharpTypeContainedRecords :
    FsharpType
    ->
        FastSet.Set
            -- sorted field names
            (List String)
fsharpTypeContainedRecords fsharpType =
    -- IGNORE TCO
    case fsharpType of
        FsharpTypeVariable _ ->
            FastSet.empty

        FsharpTypeFunction typeFunction ->
            FastSet.union
                (typeFunction.input |> fsharpTypeContainedRecords)
                (typeFunction.output |> fsharpTypeContainedRecords)

        FsharpTypeTuple typeTuple ->
            (typeTuple.part0 :: typeTuple.part1 :: typeTuple.part2Up)
                |> listMapToFastSetsAndUnify
                    fsharpTypeContainedRecords

        FsharpTypeConstruct typeConstruct ->
            -- TODO instead extract fields from the original Elm.Syntax.TypeAnnotation
            {- FsharpTypeRecord fields ->
               FastSet.insert
                   (fields |> FastDict.keys)
                   (fields
                       |> fastDictMapToFastSetAndUnify
                           fsharpTypeContainedRecords
                   )
            -}
            FastSet.union
                (if typeConstruct.name |> String.startsWith "Generated_" then
                    FastSet.singleton
                        (typeConstruct.name
                            |> String.split "_"
                            |> List.drop 1
                            |> List.filter (\field -> field /= "")
                            |> List.map stringFirstCharToUpper
                            |> List.sort
                        )

                 else
                    FastSet.empty
                )
                (typeConstruct.arguments
                    |> listMapToFastSetsAndUnify
                        fsharpTypeContainedRecords
                )


fsharpExpressionContainedConstructedRecords :
    FsharpExpression
    ->
        FastSet.Set
            -- sorted field names
            (List String)
fsharpExpressionContainedConstructedRecords syntaxExpression =
    -- IGNORE TCO
    FastSet.union
        (case syntaxExpression of
            FsharpExpressionRecord fields ->
                FastSet.singleton (fields |> FastDict.keys)

            _ ->
                FastSet.empty
        )
        (syntaxExpression
            |> fsharpExpressionSubs
            |> listMapToFastSetsAndUnify
                fsharpExpressionContainedConstructedRecords
        )


fsharpExpressionContainedLocalReferences :
    FsharpExpression
    -> FastSet.Set String
fsharpExpressionContainedLocalReferences syntaxExpression =
    -- IGNORE TCO
    case syntaxExpression of
        FsharpExpressionReference reference ->
            case reference.moduleOrigin of
                Just _ ->
                    FastSet.empty

                Nothing ->
                    FastSet.singleton reference.name

        expressionNotReference ->
            expressionNotReference
                |> fsharpExpressionSubs
                |> listMapToFastSetsAndUnify
                    fsharpExpressionContainedLocalReferences


{-| All surface-level child [expression](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression)s.
-}
fsharpExpressionSubs : FsharpExpression -> List FsharpExpression
fsharpExpressionSubs fsharpExpression =
    case fsharpExpression of
        FsharpExpressionUnit ->
            []

        FsharpExpressionCall call ->
            call.called
                :: call.arguments

        FsharpExpressionList elements ->
            elements

        FsharpExpressionRecord fields ->
            fields |> FastDict.values

        FsharpExpressionRecordUpdate recordUpdate ->
            recordUpdate.fields |> FastDict.values

        FsharpExpressionIfElse ifThenElse ->
            [ ifThenElse.condition
            , ifThenElse.onTrue
            , ifThenElse.onFalse
            ]

        FsharpExpressionWithLetDeclarations letIn ->
            List.foldr
                (\declaration soFar ->
                    case declaration of
                        FsharpLetDeclarationValueOrFunction letValueOrFunction ->
                            letValueOrFunction.result :: soFar

                        FsharpLetDestructuring letDestructuring ->
                            letDestructuring.expression :: soFar
                )
                [ letIn.result ]
                (letIn.declaration0 :: letIn.declaration1Up)

        FsharpExpressionMatchWith matchWith ->
            matchWith.matched
                :: matchWith.case0.result
                :: (matchWith.case1Up |> List.map .result)

        FsharpExpressionLambda lambda ->
            [ lambda.result ]

        FsharpExpressionTuple parts ->
            parts.part0
                :: parts.part1
                :: parts.part2Up

        FsharpExpressionRecordAccess recordAccess ->
            [ recordAccess.record ]

        FsharpExpressionInt _ ->
            []

        FsharpExpressionFloat _ ->
            []

        FsharpExpressionString _ ->
            []

        FsharpExpressionChar _ ->
            []

        FsharpExpressionReference _ ->
            []


choiceTypeDeclaration :
    ModuleContext
    -> Elm.Syntax.Type.Type
    ->
        Result
            String
            { name : String
            , parameters : List String
            , variants : FastDict.Dict String (Maybe FsharpType)
            }
choiceTypeDeclaration moduleOriginLookup syntaxChoiceType =
    Result.map
        (\variants ->
            { name =
                syntaxChoiceType.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxChoiceType.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> variableNameDisambiguateFromFsharpKeywords
                        )
            , variants = variants |> FastDict.fromList
            }
        )
        (syntaxChoiceType.constructors
            |> listMapAndCombineOk
                (\(Elm.Syntax.Node.Node _ syntaxVariant) ->
                    Result.map
                        (\values ->
                            ( syntaxVariant.name |> Elm.Syntax.Node.value
                            , case values of
                                [] ->
                                    Nothing

                                [ variantValue ] ->
                                    Just variantValue

                                variantValue0 :: variantValue1 :: variantValue2Up ->
                                    Just
                                        (FsharpTypeTuple
                                            { part0 = variantValue0
                                            , part1 = variantValue1
                                            , part2Up = variantValue2Up
                                            }
                                        )
                            )
                        )
                        (syntaxVariant.arguments
                            |> listMapAndCombineOk
                                (\value ->
                                    value |> typeAnnotation moduleOriginLookup
                                )
                        )
                )
        )


fsharpTypeParametersToString : List String -> String
fsharpTypeParametersToString fsharpTypeParameters =
    case fsharpTypeParameters of
        [] ->
            ""

        parameter0 :: parameter1Up ->
            "<"
                ++ ((parameter0 :: parameter1Up)
                        |> List.map (\parameter -> "'" ++ parameter)
                        |> String.join ", "
                   )
                ++ ">"


printFsharpChoiceTypeDeclaration :
    { name : String
    , parameters : List String
    , variants : FastDict.Dict String (Maybe FsharpType)
    }
    -> Print
printFsharpChoiceTypeDeclaration fsharpChoiceType =
    Print.exactly
        (fsharpChoiceType.name
            ++ (case fsharpChoiceType.parameters of
                    [] ->
                        ""

                    parameter0 :: parameter1Up ->
                        "<'"
                            ++ parameter0
                            ++ (parameter1Up
                                    |> List.map (\parameter -> ", '" ++ parameter)
                                    |> String.concat
                               )
                            ++ ">"
               )
            ++ " ="
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (fsharpChoiceType.variants
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( name, maybeValue ) ->
                                    printFsharpVariantDeclaration
                                        { name = name
                                        , maybeValue = maybeValue
                                        }
                                )
                                Print.linebreakIndented
                        )
                )
            )


printFsharpVariantDeclaration : { name : String, maybeValue : Maybe FsharpType } -> Print
printFsharpVariantDeclaration fsharpVariant =
    Print.exactly ("| " ++ fsharpVariant.name)
        |> Print.followedBy
            (case fsharpVariant.maybeValue of
                Nothing ->
                    Print.empty

                Just value ->
                    let
                        valuePrint : Print
                        valuePrint =
                            value |> printFsharpTypeParenthesizedIfSpaceSeparated
                    in
                    Print.exactly (" of " ++ fsharpVariant.name ++ ":")
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented
                                    (valuePrint |> Print.lineSpread)
                                    |> Print.followedBy valuePrint
                                )
                            )
            )


typeAliasDeclaration :
    ModuleContext
    -> Elm.Syntax.TypeAlias.TypeAlias
    ->
        Result
            String
            { name : String
            , parameters : List String
            , type_ : FsharpType
            }
typeAliasDeclaration moduleOriginLookup syntaxTypeAlias =
    Result.map
        (\aliasedType ->
            { name =
                syntaxTypeAlias.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxTypeAlias.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> variableNameDisambiguateFromFsharpKeywords
                        )
            , type_ = aliasedType
            }
        )
        (syntaxTypeAlias.typeAnnotation
            |> typeAnnotation moduleOriginLookup
        )


printFsharpTypeAliasDeclaration :
    { name : String
    , parameters : List String
    , type_ : FsharpType
    }
    -> Print
printFsharpTypeAliasDeclaration fsharpTypeAliasDeclaration =
    Print.exactly
        (fsharpTypeAliasDeclaration.name
            ++ (case fsharpTypeAliasDeclaration.parameters of
                    [] ->
                        ""

                    parameter0 :: parameter1Up ->
                        "<'"
                            ++ parameter0
                            ++ (parameter1Up
                                    |> List.map (\parameter -> ", '" ++ parameter)
                                    |> String.concat
                               )
                            ++ ">"
               )
            ++ " ="
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (fsharpTypeAliasDeclaration.type_
                            |> printFsharpTypeNotParenthesized
                        )
                )
            )


printFsharpRecordTypeDeclaration : List String -> Print
printFsharpRecordTypeDeclaration fsharpRecordFields =
    Print.exactly "[<Struct>]"
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy
            (Print.exactly
                ("type "
                    ++ generatedFsharpRecordTypeAliasName fsharpRecordFields
                    ++ (fsharpRecordFields
                            |> fsharpTypeParametersToString
                       )
                    ++ " ="
                )
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpTypeRecord
                            (fsharpRecordFields
                                |> List.foldl
                                    (\field soFar ->
                                        soFar
                                            |> FastDict.insert field
                                                (FsharpTypeVariable field)
                                    )
                                    FastDict.empty
                            )
                        )
                )
            )


printFsharpTypeRecord : FastDict.Dict String FsharpType -> Print
printFsharpTypeRecord syntaxRecordFields =
    if syntaxRecordFields |> FastDict.isEmpty then
        Print.exactly "{}"

    else
        let
            fieldsPrint : Print
            fieldsPrint =
                syntaxRecordFields
                    |> FastDict.toList
                    |> Print.listMapAndIntersperseAndFlatten
                        (\( fieldName, fieldValue ) ->
                            let
                                valuePrint : Print
                                valuePrint =
                                    printFsharpTypeNotParenthesized fieldValue
                            in
                            Print.withIndentIncreasedBy 2
                                (Print.exactly (fieldName ++ ":")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented
                                                (valuePrint |> Print.lineSpread)
                                                |> Print.followedBy valuePrint
                                            )
                                        )
                                )
                        )
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (Print.exactly "; ")
                        )
        in
        Print.exactly "{ "
            |> Print.followedBy fieldsPrint
            |> Print.followedBy
                (Print.spaceOrLinebreakIndented
                    (fieldsPrint |> Print.lineSpread)
                )
            |> Print.followedBy (Print.exactly "}")


fsharpTypeInt64 : FsharpType
fsharpTypeInt64 =
    FsharpTypeConstruct
        { moduleOrigin = Nothing
        , name = "int64"
        , arguments = []
        }


type_ :
    ElmSyntaxTypeInfer.Type String
    -> Result String FsharpType
type_ inferredType =
    -- IGNORE TCO
    case inferredType of
        ElmSyntaxTypeInfer.TypeVariable variable ->
            if variable |> String.startsWith "number" then
                -- assume Int
                Ok fsharpTypeInt64

            else
                Ok (FsharpTypeVariable (variable |> variableNameDisambiguateFromFsharpKeywords))

        ElmSyntaxTypeInfer.TypeNotVariable syntaxTypeNotVariable ->
            case syntaxTypeNotVariable of
                ElmSyntaxTypeInfer.TypeUnit ->
                    Ok fsharpTypeUnit

                ElmSyntaxTypeInfer.TypeConstruct typeConstruct ->
                    Result.map
                        (\arguments ->
                            let
                                fsharpReference : { moduleOrigin : Maybe String, name : String }
                                fsharpReference =
                                    case
                                        { moduleOrigin = typeConstruct.moduleOrigin
                                        , name = typeConstruct.name
                                        }
                                            |> typeConstructReferenceToCoreFsharp
                                    of
                                        Just coreFsharp ->
                                            coreFsharp

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                { moduleOrigin = typeConstruct.moduleOrigin
                                                , name = typeConstruct.name
                                                }
                                                    |> referenceToFsharpName
                                            }
                            in
                            FsharpTypeConstruct
                                { moduleOrigin = fsharpReference.moduleOrigin
                                , name = fsharpReference.name
                                , arguments = arguments
                                }
                        )
                        (typeConstruct.arguments
                            |> listMapAndCombineOk
                                (\argument -> argument |> type_)
                        )

                ElmSyntaxTypeInfer.TypeTuple parts ->
                    Result.map2
                        (\part0 part1 ->
                            FsharpTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (parts.part0 |> type_)
                        (parts.part1 |> type_)

                ElmSyntaxTypeInfer.TypeTriple parts ->
                    Result.map3
                        (\part0 part1 part2 ->
                            FsharpTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (parts.part0 |> type_)
                        (parts.part1 |> type_)
                        (parts.part2 |> type_)

                ElmSyntaxTypeInfer.TypeRecord recordFields ->
                    Result.map
                        (\fields ->
                            let
                                fieldAsFastDict : FastDict.Dict String FsharpType
                                fieldAsFastDict =
                                    FastDict.fromList fields
                            in
                            FsharpTypeConstruct
                                { moduleOrigin = Nothing
                                , name =
                                    generatedFsharpRecordTypeAliasName
                                        (fieldAsFastDict |> FastDict.keys)
                                , arguments =
                                    fieldAsFastDict
                                        |> FastDict.values
                                }
                        )
                        (recordFields
                            |> FastDict.toList
                            |> listMapAndCombineOk
                                (\( fieldName, fieldValueType ) ->
                                    Result.map
                                        (\value ->
                                            ( fieldName |> stringFirstCharToUpper
                                            , value
                                            )
                                        )
                                        (fieldValueType |> type_)
                                )
                        )

                ElmSyntaxTypeInfer.TypeFunction typeFunction ->
                    Result.map2
                        (\input output ->
                            FsharpTypeFunction
                                { input = input
                                , output = output
                                }
                        )
                        (typeFunction.input |> type_)
                        (typeFunction.output |> type_)

                ElmSyntaxTypeInfer.TypeRecordExtension typeRecordExtension ->
                    -- Err
                    --     ("extensible record types are not supported: "
                    --         ++ (ElmSyntaxTypeInfer.TypeRecordExtension typeRecordExtension
                    --                 |> typeNotVariableToInfoString
                    --            )
                    --     )
                    Result.map
                        (\fields ->
                            let
                                fieldAsFastDict : FastDict.Dict String FsharpType
                                fieldAsFastDict =
                                    FastDict.fromList fields
                            in
                            FsharpTypeConstruct
                                { moduleOrigin = Nothing
                                , name =
                                    generatedFsharpRecordTypeAliasName
                                        (fieldAsFastDict |> FastDict.keys)
                                , arguments =
                                    fieldAsFastDict
                                        |> FastDict.values
                                }
                        )
                        (typeRecordExtension.fields
                            |> FastDict.toList
                            |> listMapAndCombineOk
                                (\( fieldName, fieldValueType ) ->
                                    Result.map
                                        (\value ->
                                            ( fieldName |> stringFirstCharToUpper
                                            , value
                                            )
                                        )
                                        (fieldValueType |> type_)
                                )
                        )


typeAnnotation :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String FsharpType
typeAnnotation moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Ok fsharpTypeUnit

        Elm.Syntax.TypeAnnotation.GenericType variable ->
            Ok (FsharpTypeVariable (variable |> variableNameDisambiguateFromFsharpKeywords))

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ reference) typedArguments ->
            let
                ( qualification, name ) =
                    reference
            in
            case moduleOriginLookup.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup |> FastDict.get reference of
                Nothing ->
                    Err
                        ("could not find module origin of the type reference "
                            ++ qualifiedToString
                                { qualification = qualification
                                , name = name
                                }
                        )

                Just moduleOrigin ->
                    Result.map
                        (\arguments ->
                            let
                                fsharpReference : { moduleOrigin : Maybe String, name : String }
                                fsharpReference =
                                    case
                                        { moduleOrigin = moduleOrigin
                                        , name = name
                                        }
                                            |> typeConstructReferenceToCoreFsharp
                                    of
                                        Just coreFsharp ->
                                            coreFsharp

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                { moduleOrigin = moduleOrigin
                                                , name = name
                                                }
                                                    |> referenceToFsharpName
                                            }
                            in
                            FsharpTypeConstruct
                                { moduleOrigin = fsharpReference.moduleOrigin
                                , name = fsharpReference.name
                                , arguments = arguments
                                }
                        )
                        (typedArguments
                            |> listMapAndCombineOk
                                (\argument -> argument |> typeAnnotation moduleOriginLookup)
                        )

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Ok fsharpTypeUnit

                [ inParens ] ->
                    typeAnnotation moduleOriginLookup inParens

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            FsharpTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (tuplePart0 |> typeAnnotation moduleOriginLookup)
                        (tuplePart1 |> typeAnnotation moduleOriginLookup)

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            FsharpTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (tuplePart0 |> typeAnnotation moduleOriginLookup)
                        (tuplePart1 |> typeAnnotation moduleOriginLookup)
                        (tuplePart2 |> typeAnnotation moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.TypeAnnotation.Record recordFields ->
            Result.map
                (\fields ->
                    let
                        fieldAsFastDict : FastDict.Dict String FsharpType
                        fieldAsFastDict =
                            FastDict.fromList fields
                    in
                    FsharpTypeConstruct
                        { moduleOrigin = Nothing
                        , name =
                            generatedFsharpRecordTypeAliasName
                                (fieldAsFastDict |> FastDict.keys)
                        , arguments =
                            fieldAsFastDict
                                |> FastDict.values
                        }
                )
                (recordFields
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, valueNode )) ->
                            Result.map
                                (\value ->
                                    ( name |> stringFirstCharToUpper
                                    , value
                                    )
                                )
                                (valueNode |> typeAnnotation moduleOriginLookup)
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            Result.map2
                (\input output ->
                    FsharpTypeFunction
                        { input = input
                        , output = output
                        }
                )
                (inputNode |> typeAnnotation moduleOriginLookup)
                (outputNode |> typeAnnotation moduleOriginLookup)

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            Err "extensible record types are not supported"


printFsharpTypeNotParenthesized : FsharpType -> Print
printFsharpTypeNotParenthesized fsharpType =
    -- IGNORE TCO
    case fsharpType of
        FsharpTypeVariable variable ->
            Print.exactly ("'" ++ variable)

        FsharpTypeConstruct typeConstruct ->
            printFsharpTypeConstruct typeConstruct

        FsharpTypeTuple parts ->
            printFsharpTypeTuple parts

        FsharpTypeFunction typeFunction ->
            printFsharpTypeFunction typeFunction


printFsharpTypeFunction :
    { input : FsharpType, output : FsharpType }
    -> Print
printFsharpTypeFunction typeFunction =
    let
        inputPrint : Print
        inputPrint =
            printFsharpTypeParenthesizedIfSpaceSeparated
                typeFunction.input

        outputExpanded : List FsharpType
        outputExpanded =
            fsharpTypeExpandFunctionOutput typeFunction.output

        outputPrints : List Print
        outputPrints =
            outputExpanded
                |> List.map
                    printFsharpTypeParenthesizedIfSpaceSeparated

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            inputPrint
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        outputPrints
                            |> Print.lineSpreadListMapAndCombine
                                Print.lineSpread
                    )
    in
    inputPrint
        :: (outputPrints
                |> List.map
                    (\outputPartPrint ->
                        Print.withIndentAtNextMultipleOf4
                            outputPartPrint
                    )
           )
        |> Print.listIntersperseAndFlatten
            (Print.exactly " ->"
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented fullLineSpread)
                    )
            )


fsharpTypeExpandFunctionOutput : FsharpType -> List FsharpType
fsharpTypeExpandFunctionOutput fsharpType =
    fsharpTypeExpandFunctionIntoReverse [] fsharpType
        |> List.reverse


fsharpTypeExpandFunctionIntoReverse : List FsharpType -> FsharpType -> List FsharpType
fsharpTypeExpandFunctionIntoReverse soFarReverse fsharpType =
    case fsharpType of
        FsharpTypeFunction function ->
            fsharpTypeExpandFunctionIntoReverse
                (function.input :: soFarReverse)
                function.output

        FsharpTypeConstruct construct ->
            FsharpTypeConstruct construct :: soFarReverse

        FsharpTypeTuple parts ->
            FsharpTypeTuple parts :: soFarReverse

        FsharpTypeVariable variable ->
            FsharpTypeVariable variable :: soFarReverse


printFsharpTypeTuple :
    { part0 : FsharpType, part1 : FsharpType, part2Up : List FsharpType }
    -> Print
printFsharpTypeTuple parts =
    let
        part0Print : Print
        part0Print =
            parts.part0 |> printFsharpTypeNotParenthesized

        part1Print : Print
        part1Print =
            parts.part1 |> printFsharpTypeNotParenthesized

        part2UpPrints : List Print
        part2UpPrints =
            parts.part2Up
                |> List.map printFsharpTypeNotParenthesized
    in
    Print.exactly "(struct("
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                Print.linebreakIndented
            )
        |> Print.followedBy
            ((part0Print :: part1Print :: part2UpPrints)
                |> Print.listMapAndIntersperseAndFlatten
                    (\partPrint ->
                        Print.withIndentIncreasedBy 2
                            partPrint
                    )
                    (Print.linebreakIndented
                        |> Print.followedBy
                            (Print.exactly "* ")
                    )
            )
        |> Print.followedBy
            Print.linebreakIndented
        |> Print.followedBy (Print.exactly "))")


printFsharpTypeConstruct :
    { moduleOrigin : Maybe String
    , name : String
    , arguments : List FsharpType
    }
    -> Print
printFsharpTypeConstruct typeConstruct =
    let
        referencePrint : Print
        referencePrint =
            Print.exactly
                (fsharpReferenceToString
                    { moduleOrigin = typeConstruct.moduleOrigin
                    , name = typeConstruct.name
                    }
                )
    in
    case typeConstruct.arguments of
        [] ->
            referencePrint

        argument0 :: argument1Up ->
            let
                argumentPrints : List Print
                argumentPrints =
                    (argument0 :: argument1Up)
                        |> List.map printFsharpTypeNotParenthesized

                fullLineSpread : Print.LineSpread
                fullLineSpread =
                    argumentPrints
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
            in
            referencePrint
                |> Print.followedBy (Print.exactly "<")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.emptyOrLinebreakIndented fullLineSpread
                            |> Print.followedBy
                                (argumentPrints
                                    |> Print.listIntersperseAndFlatten
                                        (Print.exactly ","
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented fullLineSpread)
                                        )
                                )
                            |> Print.followedBy
                                (Print.emptyOrLinebreakIndented fullLineSpread)
                            |> Print.followedBy
                                (Print.exactly ">")
                        )
                    )


typeIsSpaceSeparated : FsharpType -> Bool
typeIsSpaceSeparated fsharpType =
    case fsharpType of
        FsharpTypeVariable _ ->
            False

        FsharpTypeConstruct _ ->
            False

        FsharpTypeTuple _ ->
            False

        FsharpTypeFunction _ ->
            True


printFsharpTypeParenthesizedIfSpaceSeparated : FsharpType -> Print
printFsharpTypeParenthesizedIfSpaceSeparated fsharpType =
    if fsharpType |> typeIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = fsharpType |> printFsharpTypeNotParenthesized
            }

    else
        fsharpType |> printFsharpTypeNotParenthesized


floatLiteral : Float -> String
floatLiteral float =
    let
        asString : String
        asString =
            float |> String.fromFloat
    in
    if asString |> String.contains "." then
        asString

    else
        asString ++ ".0"


intLiteral : Int -> String
intLiteral int =
    (int |> String.fromInt) ++ "L"


fsharpReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
fsharpReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleName ->
            moduleName
                ++ "."
                ++ reference.name


qualifiedToString :
    { qualification : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
qualifiedToString reference =
    case reference.qualification of
        [] ->
            reference.name

        qualificationPart0 :: qualificationPart1Up ->
            ((qualificationPart0 :: qualificationPart1Up)
                |> String.join "."
            )
                ++ "."
                ++ reference.name


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toLower headChar) tailString


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString


stringFirstCharIsUpper : String -> Bool
stringFirstCharIsUpper string =
    case string |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isUpper


printFsharpString : String -> Print
printFsharpString stringContent =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                    )
                    ""
    in
    -- TODO integrate more nicely
    Print.exactly ("(StringRopeOne \"" ++ singleDoubleQuotedStringContentEscaped ++ "\")")


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '$' ->
            "\\$"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u" ++ characterHex otherCharacter

            else
                String.fromChar otherCharacter


intToHexString : Int -> String
intToHexString int =
    -- IGNORE TCO
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)
            ++ ""


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    let
        charCode : Int
        charCode =
            Char.toCode character
    in
    String.toUpper
        (unsafeHexDigitIntToString
            (charCode
                |> Bitwise.and 0xF000
                |> Bitwise.shiftRightBy 12
            )
            ++ unsafeHexDigitIntToString
                (charCode
                    |> Bitwise.and 0x0F00
                    |> Bitwise.shiftRightBy 8
                )
            ++ unsafeHexDigitIntToString
                (charCode
                    |> Bitwise.and 0xF0
                    |> Bitwise.shiftRightBy 4
                )
            ++ unsafeHexDigitIntToString
                (charCode |> Bitwise.and 0x0F)
            ++ ""
        )


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        charIsLatinAlphaNumOrUnderscoreFast character
            || (character == ' ')
            || (character == '.')
            || (character == '!')
            || (character == '?')
            || (character == '-')
            || (character == ':')
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

            Just category ->
                case category of
                    Unicode.SeparatorLine ->
                        True

                    Unicode.SeparatorParagraph ->
                        True

                    Unicode.OtherControl ->
                        True

                    Unicode.OtherFormat ->
                        True

                    Unicode.OtherSurrogate ->
                        True

                    Unicode.OtherPrivateUse ->
                        True

                    Unicode.OtherNotAssigned ->
                        True

                    Unicode.LetterUppercase ->
                        False

                    Unicode.LetterLowercase ->
                        False

                    Unicode.LetterTitlecase ->
                        False

                    Unicode.MarkNonSpacing ->
                        False

                    Unicode.MarkSpacingCombining ->
                        False

                    Unicode.MarkEnclosing ->
                        False

                    Unicode.NumberDecimalDigit ->
                        False

                    Unicode.NumberLetter ->
                        False

                    Unicode.NumberOther ->
                        False

                    Unicode.SeparatorSpace ->
                        True

                    Unicode.LetterModifier ->
                        False

                    Unicode.LetterOther ->
                        False

                    Unicode.PunctuationConnector ->
                        False

                    Unicode.PunctuationDash ->
                        False

                    Unicode.PunctuationOpen ->
                        False

                    Unicode.PunctuationClose ->
                        False

                    Unicode.PunctuationInitialQuote ->
                        False

                    Unicode.PunctuationFinalQuote ->
                        False

                    Unicode.PunctuationOther ->
                        False

                    Unicode.SymbolMath ->
                        False

                    Unicode.SymbolCurrency ->
                        False

                    Unicode.SymbolModifier ->
                        False

                    Unicode.SymbolOther ->
                        False


charLiteral : Char -> String
charLiteral charContent =
    "'"
        ++ quotedCharToEscaped charContent
        ++ "'"


quotedCharToEscaped : Char -> String
quotedCharToEscaped character =
    case character of
        '\'' ->
            "\\'"

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u" ++ characterHex otherCharacter

            else
                String.fromChar otherCharacter


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code


charIsLatinAlphaNumOrUnderscoreFast : Char -> Bool
charIsLatinAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || -- (c == '_')
           (code == 95)


pattern :
    ElmSyntaxTypeInfer.TypedNode
        (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
        (ElmSyntaxTypeInfer.Type String)
    ->
        Result
            String
            { pattern : FsharpPattern
            , introducedVariables : FastSet.Set String
            }
pattern patternInferred =
    -- IGNORE TCO
    case patternInferred.value of
        ElmSyntaxTypeInfer.PatternIgnored ->
            Ok
                { pattern = FsharpPatternIgnore
                , introducedVariables = FastSet.empty
                }

        ElmSyntaxTypeInfer.PatternUnit ->
            Ok
                { pattern = FsharpPatternIgnore
                , introducedVariables = FastSet.empty
                }

        ElmSyntaxTypeInfer.PatternChar charValue ->
            Ok
                { pattern = FsharpPatternChar charValue
                , introducedVariables = FastSet.empty
                }

        ElmSyntaxTypeInfer.PatternString stringValue ->
            Ok
                { pattern = FsharpPatternString stringValue
                , introducedVariables = FastSet.empty
                }

        ElmSyntaxTypeInfer.PatternInt intValue ->
            Ok
                { pattern = FsharpPatternInt intValue.value
                , introducedVariables = FastSet.empty
                }

        ElmSyntaxTypeInfer.PatternVariable variableName ->
            let
                disambiguatedVariableName : String
                disambiguatedVariableName =
                    variableName |> variableNameDisambiguateFromFsharpKeywords
            in
            Ok
                { pattern =
                    FsharpPatternVariable disambiguatedVariableName
                , introducedVariables =
                    FastSet.singleton disambiguatedVariableName
                }

        ElmSyntaxTypeInfer.PatternParenthesized inParens ->
            pattern inParens

        ElmSyntaxTypeInfer.PatternTuple parts ->
            Result.map2
                (\part0 part1 ->
                    { pattern =
                        FsharpPatternTuple
                            { part0 = part0.pattern
                            , part1 = part1.pattern
                            , part2Up = []
                            }
                    , introducedVariables =
                        FastSet.union
                            part0.introducedVariables
                            part1.introducedVariables
                    }
                )
                (parts.part0 |> pattern)
                (parts.part1 |> pattern)

        ElmSyntaxTypeInfer.PatternTriple parts ->
            Result.map3
                (\part0 part1 part2 ->
                    { pattern =
                        FsharpPatternTuple
                            { part0 = part0.pattern
                            , part1 = part1.pattern
                            , part2Up = [ part2.pattern ]
                            }
                    , introducedVariables =
                        FastSet.union
                            part0.introducedVariables
                            (FastSet.union
                                part1.introducedVariables
                                part2.introducedVariables
                            )
                    }
                )
                (parts.part0 |> pattern)
                (parts.part1 |> pattern)
                (parts.part2 |> pattern)

        ElmSyntaxTypeInfer.PatternRecord fields ->
            let
                fieldNames :
                    { fields : FastDict.Dict String FsharpPattern
                    , introducedVariables : FastSet.Set String
                    }
                fieldNames =
                    fields
                        |> List.foldl
                            (\fieldTypedNode soFar ->
                                let
                                    introducedVariableName : String
                                    introducedVariableName =
                                        fieldTypedNode.value
                                            |> variableNameDisambiguateFromFsharpKeywords
                                in
                                { fields =
                                    soFar.fields
                                        |> FastDict.insert
                                            (fieldTypedNode.value |> stringFirstCharToUpper)
                                            (FsharpPatternVariable
                                                introducedVariableName
                                            )
                                , introducedVariables =
                                    soFar.introducedVariables
                                        |> FastSet.insert introducedVariableName
                                }
                            )
                            { fields = FastDict.empty
                            , introducedVariables = FastSet.empty
                            }
            in
            Ok
                { pattern = FsharpPatternRecordInexhaustive fieldNames.fields
                , introducedVariables = fieldNames.introducedVariables
                }

        ElmSyntaxTypeInfer.PatternListCons listCons ->
            resultAndThen2
                (\head tail ->
                    let
                        introducedVariables : FastSet.Set String
                        introducedVariables =
                            FastSet.union
                                head.introducedVariables
                                tail.introducedVariables
                    in
                    Ok
                        { pattern =
                            FsharpPatternListCons
                                { head = head.pattern
                                , tail = tail.pattern
                                }
                        , introducedVariables = introducedVariables
                        }
                )
                (listCons.head |> pattern)
                (listCons.tail |> pattern)

        ElmSyntaxTypeInfer.PatternListExact elementPatterns ->
            Result.map
                (\elements ->
                    { pattern =
                        FsharpPatternListExact (elements |> List.map .pattern)
                    , introducedVariables =
                        elements
                            |> listMapToFastSetsAndUnify .introducedVariables
                    }
                )
                (elementPatterns
                    |> listMapAndCombineOk
                        (\element -> element |> pattern)
                )

        ElmSyntaxTypeInfer.PatternVariant variant ->
            Result.map
                (\values ->
                    let
                        reference : { moduleOrigin : Maybe String, name : String }
                        reference =
                            case
                                { moduleOrigin = variant.moduleOrigin
                                , name = variant.name
                                , type_ = patternInferred.type_
                                }
                                    |> referenceToCoreFsharp
                            of
                                Just fsharpReference ->
                                    fsharpReference

                                Nothing ->
                                    { moduleOrigin = Nothing
                                    , name =
                                        referenceToFsharpName
                                            { moduleOrigin = variant.moduleOrigin
                                            , name = variant.name
                                            }
                                    }
                    in
                    { pattern =
                        FsharpPatternVariant
                            { moduleOrigin = reference.moduleOrigin
                            , name = reference.name
                            , values = values |> List.map .pattern
                            }
                    , introducedVariables =
                        values
                            |> listMapToFastSetsAndUnify .introducedVariables
                    }
                )
                (variant.values
                    |> listMapAndCombineOk
                        (\argument -> argument |> pattern)
                )

        ElmSyntaxTypeInfer.PatternAs patternAs ->
            Result.map
                (\aliasedPattern ->
                    let
                        variableDisambiguated : String
                        variableDisambiguated =
                            patternAs.variable.value |> variableNameDisambiguateFromFsharpKeywords
                    in
                    { pattern =
                        FsharpPatternAs
                            { pattern = aliasedPattern.pattern
                            , variable = variableDisambiguated
                            }
                    , introducedVariables =
                        aliasedPattern.introducedVariables
                            |> FastSet.insert variableDisambiguated
                    }
                )
                (patternAs.pattern |> pattern)


typedPattern :
    ElmSyntaxTypeInfer.TypedNode
        (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
        (ElmSyntaxTypeInfer.Type String)
    ->
        Result
            String
            { pattern : FsharpPattern
            , type_ : FsharpType
            , introducedVariables : FastSet.Set String
            }
typedPattern patternTypedNode =
    Result.map2
        (\fsharpPattern fsharpType ->
            { pattern = fsharpPattern.pattern
            , type_ = fsharpType
            , introducedVariables = fsharpPattern.introducedVariables
            }
        )
        (patternTypedNode |> pattern)
        (patternTypedNode.type_ |> type_)


printFsharpPatternListCons :
    { head : FsharpPattern
    , tail : FsharpPattern
    }
    -> Print
printFsharpPatternListCons syntaxCons =
    printFsharpPatternParenthesizedIfSpaceSeparated
        syntaxCons.head
        |> Print.followedBy
            (Print.exactly " :: ")
        |> Print.followedBy
            (printFsharpPatternParenthesizedIfSpaceSeparated
                syntaxCons.tail
            )


typeConstructReferenceToCoreFsharp :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    ->
        Maybe
            { moduleOrigin : Maybe String
            , name : String
            }
typeConstructReferenceToCoreFsharp reference =
    case reference.moduleOrigin of
        [ "Basics" ] ->
            case reference.name of
                "Order" ->
                    Just { moduleOrigin = Nothing, name = "Basics_Order" }

                "Bool" ->
                    Just { moduleOrigin = Nothing, name = "bool" }

                "Int" ->
                    Just { moduleOrigin = Nothing, name = "int64" }

                "Float" ->
                    Just { moduleOrigin = Nothing, name = "float" }

                "Never" ->
                    Just { moduleOrigin = Nothing, name = "Basics_Never" }

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "String" ->
                    Just { moduleOrigin = Nothing, name = "StringRope" }

                _ ->
                    Nothing

        [ "Char" ] ->
            case reference.name of
                "Char" ->
                    Just { moduleOrigin = Nothing, name = "char" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "List" ->
                    Just { moduleOrigin = Nothing, name = "List" }

                _ ->
                    Nothing

        [ "Dict" ] ->
            case reference.name of
                "Dict" ->
                    Just { moduleOrigin = Nothing, name = "Map" }

                _ ->
                    Nothing

        [ "Set" ] ->
            case reference.name of
                "Set" ->
                    Just { moduleOrigin = Nothing, name = "Set" }

                _ ->
                    Nothing

        [ "Array" ] ->
            case reference.name of
                "Array" ->
                    Just { moduleOrigin = Nothing, name = "array" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Maybe" ->
                    Just { moduleOrigin = Nothing, name = "option" }

                _ ->
                    Nothing

        [ "Result" ] ->
            case reference.name of
                "Result" ->
                    Just { moduleOrigin = Nothing, name = "Result_Result" }

                _ ->
                    Nothing

        [ "Json", "Encode" ] ->
            case reference.name of
                "Value" ->
                    Just { moduleOrigin = Just "System.Text.Json.Nodes", name = "JsonNode" }

                _ ->
                    Nothing

        [ "Json", "Decode" ] ->
            case reference.name of
                "Value" ->
                    Just { moduleOrigin = Just "System.Text.Json.Nodes", name = "JsonNode" }

                "Decoder" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_Decoder" }

                "Error" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_Error" }

                _ ->
                    Nothing

        [ "Regex" ] ->
            case reference.name of
                "Regex" ->
                    Just { moduleOrigin = Just "System.Text.RegularExpressions", name = "Regex" }

                "Options" ->
                    Just { moduleOrigin = Nothing, name = "Regex_Options" }

                "Match" ->
                    Just { moduleOrigin = Nothing, name = "Regex_Match" }

                _ ->
                    Nothing

        [ "Random" ] ->
            case reference.name of
                "Seed" ->
                    Just { moduleOrigin = Nothing, name = "Random_Seed" }

                "Generator" ->
                    Just { moduleOrigin = Nothing, name = "Random_Generator" }

                _ ->
                    Nothing

        [ "Time" ] ->
            case reference.name of
                "Posix" ->
                    Just { moduleOrigin = Nothing, name = "Time_Posix" }

                "Zone" ->
                    Just { moduleOrigin = Nothing, name = "Time_Zone" }

                "Month" ->
                    Just { moduleOrigin = Nothing, name = "Time_Month" }

                "Weekday" ->
                    Just { moduleOrigin = Nothing, name = "Time_Weekday" }

                "Time_ZoneName" ->
                    Just { moduleOrigin = Nothing, name = "Time_ZoneName" }

                _ ->
                    Nothing

        [ "Bytes" ] ->
            case reference.name of
                "Endianness" ->
                    Just { moduleOrigin = Nothing, name = "Bytes_Endianness" }

                "Bytes" ->
                    Just { moduleOrigin = Nothing, name = "Bytes_Bytes" }

                _ ->
                    Nothing

        [ "Bytes", "Decode" ] ->
            case reference.name of
                "Decoder" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_Decoder" }

                "Step" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_Step" }

                _ ->
                    Nothing

        [ "Bytes", "Encode" ] ->
            case reference.name of
                "Encoder" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_Encoder" }

                _ ->
                    Nothing

        [ "Platform" ] ->
            case reference.name of
                "Program" ->
                    Just { moduleOrigin = Nothing, name = "Platform_Program" }

                _ ->
                    Nothing

        [ "Platform", "Cmd" ] ->
            case reference.name of
                "Cmd" ->
                    Just { moduleOrigin = Nothing, name = "PlatformCmd_Cmd" }

                _ ->
                    Nothing

        [ "Platform", "Sub" ] ->
            case reference.name of
                "Sub" ->
                    Just { moduleOrigin = Nothing, name = "PlatformSub_Sub" }

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Use `typeConstructReferenceToCoreFsharp` for types
-}
referenceToCoreFsharp :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    , type_ : ElmSyntaxTypeInfer.Type String
    }
    ->
        Maybe
            { moduleOrigin : Maybe String
            , name : String
            }
referenceToCoreFsharp reference =
    case reference.moduleOrigin of
        [ "Basics" ] ->
            case reference.name of
                "identity" ->
                    Just { moduleOrigin = Nothing, name = "id" }

                "always" ->
                    Just { moduleOrigin = Nothing, name = "Basics_always" }

                "compare" ->
                    Just { moduleOrigin = Nothing, name = "Basics_compare" }

                "max" ->
                    Just { moduleOrigin = Nothing, name = "max" }

                "min" ->
                    Just { moduleOrigin = Nothing, name = "min" }

                "LT" ->
                    Just { moduleOrigin = Just "Basics_Order", name = "LT" }

                "EQ" ->
                    Just { moduleOrigin = Just "Basics_Order", name = "EQ" }

                "GT" ->
                    Just { moduleOrigin = Just "Basics_Order", name = "GT" }

                "True" ->
                    Just { moduleOrigin = Nothing, name = "true" }

                "False" ->
                    Just { moduleOrigin = Nothing, name = "false" }

                "not" ->
                    Just { moduleOrigin = Nothing, name = "not" }

                "xor" ->
                    Just { moduleOrigin = Nothing, name = "Basics_neq" }

                "e" ->
                    Just { moduleOrigin = Just "System.Math", name = "E" }

                "pi" ->
                    Just { moduleOrigin = Just "System.Math", name = "Pi" }

                "ceiling" ->
                    Just { moduleOrigin = Nothing, name = "Basics_ceiling" }

                "floor" ->
                    Just { moduleOrigin = Nothing, name = "Basics_floor" }

                "round" ->
                    Just { moduleOrigin = Nothing, name = "Basics_round" }

                "truncate" ->
                    Just { moduleOrigin = Nothing, name = "truncate" }

                "negate" ->
                    case reference.type_ of
                        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                            case typeFunction.input of
                                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct inputTypeConstruct) ->
                                    case ( inputTypeConstruct.moduleOrigin, inputTypeConstruct.name ) of
                                        ( [ "Basics" ], "Float" ) ->
                                            Just { moduleOrigin = Nothing, name = "Basics_fnegate" }

                                        _ ->
                                            -- assume Int
                                            Just { moduleOrigin = Nothing, name = "Basics_inegate" }

                                _ ->
                                    -- assume Int
                                    Just { moduleOrigin = Nothing, name = "Basics_inegate" }

                        _ ->
                            -- assume Int
                            Just { moduleOrigin = Nothing, name = "Basics_inegate" }

                "abs" ->
                    case reference.type_ of
                        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                            case typeFunction.input of
                                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct inputTypeConstruct) ->
                                    case ( inputTypeConstruct.moduleOrigin, inputTypeConstruct.name ) of
                                        ( [ "Basics" ], "Float" ) ->
                                            Just { moduleOrigin = Nothing, name = "Basics_fabs" }

                                        _ ->
                                            -- assume Int
                                            Just { moduleOrigin = Nothing, name = "Basics_iabs" }

                                _ ->
                                    -- assume Int
                                    Just { moduleOrigin = Nothing, name = "Basics_iabs" }

                        _ ->
                            -- assume Int
                            Just { moduleOrigin = Nothing, name = "Basics_iabs" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "float" }

                "isNaN" ->
                    Just { moduleOrigin = Just "System.Double", name = "IsNaN" }

                "isInfinite" ->
                    Just { moduleOrigin = Just "System.Double", name = "IsInfinity" }

                "remainderBy" ->
                    Just { moduleOrigin = Nothing, name = "Basics_remainderBy" }

                "modBy" ->
                    Just { moduleOrigin = Nothing, name = "Basics_modBy" }

                "sin" ->
                    Just { moduleOrigin = Just "System.Double", name = "Sin" }

                "cos" ->
                    Just { moduleOrigin = Just "System.Double", name = "Cos" }

                "tan" ->
                    Just { moduleOrigin = Just "System.Double", name = "Tan" }

                "asin" ->
                    Just { moduleOrigin = Just "System.Double", name = "Asin" }

                "acos" ->
                    Just { moduleOrigin = Just "System.Double", name = "Acos" }

                "atan" ->
                    Just { moduleOrigin = Just "System.Double", name = "Atan" }

                "atan2" ->
                    Just { moduleOrigin = Nothing, name = "Basics_atan2" }

                "sqrt" ->
                    Just { moduleOrigin = Just "System.Double", name = "Sqrt" }

                "logBase" ->
                    Just { moduleOrigin = Nothing, name = "Basics_logBase" }

                "radians" ->
                    Just { moduleOrigin = Nothing, name = "Basics_radians" }

                "degrees" ->
                    Just { moduleOrigin = Nothing, name = "Basics_degrees" }

                "turns" ->
                    Just { moduleOrigin = Nothing, name = "Basics_turns" }

                "fromPolar" ->
                    Just { moduleOrigin = Nothing, name = "Basics_fromPolar" }

                "toPolar" ->
                    Just { moduleOrigin = Nothing, name = "Basics_toPolar" }

                "clamp" ->
                    case reference.type_ of
                        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                            if
                                typeFunction.input
                                    == ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ], name = "Float", arguments = [] }
                                        )
                            then
                                Just { moduleOrigin = Nothing, name = "Basics_fclamp" }

                            else
                                -- assume Int
                                Just { moduleOrigin = Nothing, name = "Basics_iclamp" }

                        _ ->
                            -- assume Int
                            Just { moduleOrigin = Nothing, name = "Basics_iclamp" }

                "never" ->
                    Just { moduleOrigin = Nothing, name = "Basics_never" }

                _ ->
                    Nothing

        [ "Bitwise" ] ->
            case reference.name of
                "complement" ->
                    Just { moduleOrigin = Nothing, name = "(~~~)" }

                "and" ->
                    Just { moduleOrigin = Nothing, name = "(&&&)" }

                "or" ->
                    Just { moduleOrigin = Nothing, name = "(|||)" }

                "xor" ->
                    Just { moduleOrigin = Nothing, name = "(^^^)" }

                "shiftLeftBy" ->
                    Just { moduleOrigin = Nothing, name = "Bitwise_shiftLeftBy" }

                "shiftRightBy" ->
                    Just { moduleOrigin = Nothing, name = "Bitwise_shiftRightBy" }

                "shiftRightZfBy" ->
                    Just { moduleOrigin = Nothing, name = "Bitwise_shiftRightZfBy" }

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "isEmpty" ->
                    Just { moduleOrigin = Nothing, name = "String_isEmpty" }

                "length" ->
                    Just { moduleOrigin = Nothing, name = "String_length" }

                "append" ->
                    Just { moduleOrigin = Nothing, name = "String_concat" }

                "trim" ->
                    Just { moduleOrigin = Nothing, name = "String_trim" }

                "trimLeft" ->
                    Just { moduleOrigin = Nothing, name = "String_trimLeft" }

                "trimRight" ->
                    Just { moduleOrigin = Nothing, name = "String_trimRight" }

                "left" ->
                    Just { moduleOrigin = Nothing, name = "String_left" }

                "right" ->
                    Just { moduleOrigin = Nothing, name = "String_right" }

                "dropLeft" ->
                    Just { moduleOrigin = Nothing, name = "String_dropLeft" }

                "dropRight" ->
                    Just { moduleOrigin = Nothing, name = "String_dropRight" }

                "padLeft" ->
                    Just { moduleOrigin = Nothing, name = "String_padLeft" }

                "padRight" ->
                    Just { moduleOrigin = Nothing, name = "String_padRight" }

                "replace" ->
                    Just { moduleOrigin = Nothing, name = "String_replace" }

                "toList" ->
                    Just { moduleOrigin = Nothing, name = "String_toList" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "String_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "String_foldr" }

                "join" ->
                    Just { moduleOrigin = Nothing, name = "String_join" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "String_filter" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "String_any" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "String_all" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "String_map" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "String_repeat" }

                "split" ->
                    Just { moduleOrigin = Nothing, name = "String_split" }

                "lines" ->
                    Just { moduleOrigin = Nothing, name = "String_lines" }

                "startsWith" ->
                    Just { moduleOrigin = Nothing, name = "String_startsWith" }

                "endsWith" ->
                    Just { moduleOrigin = Nothing, name = "String_endsWith" }

                "toInt" ->
                    Just { moduleOrigin = Nothing, name = "String_toInt" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "String_toFloat" }

                "fromInt" ->
                    Just { moduleOrigin = Nothing, name = "String_fromInt" }

                "fromFloat" ->
                    Just { moduleOrigin = Nothing, name = "String_fromFloat" }

                "contains" ->
                    Just { moduleOrigin = Nothing, name = "String_contains" }

                "fromChar" ->
                    Just { moduleOrigin = Nothing, name = "String_fromChar" }

                "cons" ->
                    Just { moduleOrigin = Nothing, name = "String_cons" }

                "uncons" ->
                    Just { moduleOrigin = Nothing, name = "String_uncons" }

                "slice" ->
                    Just { moduleOrigin = Nothing, name = "String_slice" }

                "toLower" ->
                    Just { moduleOrigin = Nothing, name = "String_toLower" }

                "toUpper" ->
                    Just { moduleOrigin = Nothing, name = "String_toUpper" }

                _ ->
                    Nothing

        [ "Char" ] ->
            case reference.name of
                "toCode" ->
                    Just { moduleOrigin = Nothing, name = "int64" }

                "fromCode" ->
                    Just { moduleOrigin = Nothing, name = "char" }

                "toLower" ->
                    Just { moduleOrigin = Just "System.Char", name = "ToLowerInvariant" }

                "toUpper" ->
                    Just { moduleOrigin = Just "System.Char", name = "ToUpperInvariant" }

                "toLocaleLower" ->
                    Just { moduleOrigin = Just "System.Char", name = "ToLower" }

                "toLocaleUpper" ->
                    Just { moduleOrigin = Just "System.Char", name = "ToUpper" }

                "isLower" ->
                    Just { moduleOrigin = Just "System.Char", name = "IsAsciiLetterLower" }

                "isUpper" ->
                    Just { moduleOrigin = Just "System.Char", name = "IsAsciiLetterUpper" }

                "isHexDigit" ->
                    Just { moduleOrigin = Just "System.Char", name = "IsAsciiHexDigit" }

                "isOctDigit" ->
                    Just { moduleOrigin = Just "System.Char", name = "Char_isOctDigit" }

                "isDigit" ->
                    Just { moduleOrigin = Just "System.Char", name = "IsAsciiDigit" }

                "isAlpha" ->
                    Just { moduleOrigin = Just "System.Char", name = "IsAsciiLetter" }

                "isAlphaNum" ->
                    Just { moduleOrigin = Just "System.Char", name = "IsAsciiLetterOrDigit" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "singleton" ->
                    Just { moduleOrigin = Just "List", name = "singleton" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "List", name = "isEmpty" }

                "length" ->
                    Just { moduleOrigin = Nothing, name = "List_length" }

                "member" ->
                    Just { moduleOrigin = Nothing, name = "List_member" }

                "minimum" ->
                    Just { moduleOrigin = Nothing, name = "List_minimum" }

                "maximum" ->
                    Just { moduleOrigin = Nothing, name = "List_maximum" }

                "sum" ->
                    Just { moduleOrigin = Just "List", name = "sum" }

                "product" ->
                    case reference.type_ of
                        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                            case typeFunction.input of
                                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct inputTypeConstruct) ->
                                    case ( inputTypeConstruct.moduleOrigin, inputTypeConstruct.name ) of
                                        ( [ "List" ], "List" ) ->
                                            case typeFunction.input of
                                                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct inputListElementTypeConstruct) ->
                                                    case ( inputListElementTypeConstruct.moduleOrigin, inputListElementTypeConstruct.name ) of
                                                        ( [ "Basics" ], "Float" ) ->
                                                            Just { moduleOrigin = Nothing, name = "List_fproduct" }

                                                        _ ->
                                                            -- assume List Int
                                                            Just { moduleOrigin = Nothing, name = "List_iproduct" }

                                                _ ->
                                                    -- assume List Int
                                                    Just { moduleOrigin = Nothing, name = "List_iproduct" }

                                        _ ->
                                            -- assume List Int
                                            Just { moduleOrigin = Nothing, name = "List_iproduct" }

                                _ ->
                                    -- assume List Int
                                    Just { moduleOrigin = Nothing, name = "List_iproduct" }

                        _ ->
                            -- assume List Int
                            Just { moduleOrigin = Nothing, name = "List_iproduct" }

                "append" ->
                    Just { moduleOrigin = Just "List", name = "append" }

                "concat" ->
                    Just { moduleOrigin = Just "List", name = "join" }

                "reverse" ->
                    Just { moduleOrigin = Just "List", name = "rev" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "List_repeat" }

                "head" ->
                    Just { moduleOrigin = Just "List", name = "tryHead" }

                "tail" ->
                    Just { moduleOrigin = Nothing, name = "List_tail" }

                "all" ->
                    Just { moduleOrigin = Just "List", name = "forall" }

                "any" ->
                    Just { moduleOrigin = Just "List", name = "exists" }

                "filter" ->
                    Just { moduleOrigin = Just "List", name = "filter" }

                "filterMap" ->
                    Just { moduleOrigin = Just "List", name = "choose" }

                "map" ->
                    Just { moduleOrigin = Just "List", name = "map" }

                "indexedMap" ->
                    Just { moduleOrigin = Just "List", name = "mapi" }

                "map2" ->
                    Just { moduleOrigin = Just "List", name = "map2" }

                "zip" ->
                    Just { moduleOrigin = Just "List", name = "zip" }

                "map3" ->
                    Just { moduleOrigin = Just "List", name = "map3" }

                "map4" ->
                    Just { moduleOrigin = Nothing, name = "List_map4" }

                "map5" ->
                    Just { moduleOrigin = Nothing, name = "List_map5" }

                "unzip" ->
                    Just { moduleOrigin = Just "List", name = "unzip" }

                "concatMap" ->
                    Just { moduleOrigin = Just "List", name = "collect" }

                "sort" ->
                    Just { moduleOrigin = Just "List", name = "sort" }

                "sortBy" ->
                    Just { moduleOrigin = Just "List", name = "sortBy" }

                "sortWith" ->
                    Just { moduleOrigin = Nothing, name = "List_sortWith" }

                "range" ->
                    Just { moduleOrigin = Nothing, name = "List_range" }

                "take" ->
                    Just { moduleOrigin = Nothing, name = "List_take" }

                "drop" ->
                    Just { moduleOrigin = Nothing, name = "List_drop" }

                "intersperse" ->
                    Just { moduleOrigin = Nothing, name = "List_intersperse" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "List_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "List_foldr" }

                _ ->
                    Nothing

        [ "Dict" ] ->
            case reference.name of
                "size" ->
                    Just { moduleOrigin = Nothing, name = "Dict_size" }

                "empty" ->
                    Just { moduleOrigin = Just "Map", name = "empty" }

                "singleton" ->
                    Just { moduleOrigin = Nothing, name = "Dict_singleton" }

                "fromList" ->
                    Just { moduleOrigin = Nothing, name = "Dict_fromList" }

                "toList" ->
                    Just { moduleOrigin = Nothing, name = "Dict_toList" }

                "keys" ->
                    Just { moduleOrigin = Nothing, name = "Dict_keys" }

                "values" ->
                    Just { moduleOrigin = Nothing, name = "Dict_values" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "Map", name = "isEmpty" }

                "map" ->
                    Just { moduleOrigin = Just "Map", name = "map" }

                "partition" ->
                    Just { moduleOrigin = Just "Map", name = "partition" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "Dict_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "Dict_foldr" }

                "filter" ->
                    Just { moduleOrigin = Just "Map", name = "filter" }

                "get" ->
                    Just { moduleOrigin = Just "Map", name = "tryFind" }

                "member" ->
                    Just { moduleOrigin = Just "Map", name = "containsKey" }

                "insert" ->
                    Just { moduleOrigin = Just "Map", name = "add" }

                "update" ->
                    Just { moduleOrigin = Just "Map", name = "change" }

                "remove" ->
                    Just { moduleOrigin = Just "Map", name = "remove" }

                "union" ->
                    Just { moduleOrigin = Nothing, name = "Dict_union" }

                "diff" ->
                    Just { moduleOrigin = Nothing, name = "Dict_diff" }

                "intersect" ->
                    Just { moduleOrigin = Nothing, name = "Dict_intersect" }

                "merge" ->
                    Just { moduleOrigin = Nothing, name = "Dict_merge" }

                _ ->
                    Nothing

        [ "Set" ] ->
            case reference.name of
                "size" ->
                    Just { moduleOrigin = Nothing, name = "Set_size" }

                "empty" ->
                    Just { moduleOrigin = Just "Set", name = "empty" }

                "singleton" ->
                    Just { moduleOrigin = Just "Set", name = "singleton" }

                "fromList" ->
                    Just { moduleOrigin = Just "Set", name = "ofList" }

                "toList" ->
                    Just { moduleOrigin = Just "Set", name = "toList" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "Set", name = "isEmpty" }

                "insert" ->
                    Just { moduleOrigin = Just "Set", name = "add" }

                "partition" ->
                    Just { moduleOrigin = Just "Set", name = "partition" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "Set_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "Set_foldr" }

                "filter" ->
                    Just { moduleOrigin = Just "Set", name = "filter" }

                "member" ->
                    Just { moduleOrigin = Just "Set", name = "contains" }

                "remove" ->
                    Just { moduleOrigin = Just "Set", name = "remove" }

                "union" ->
                    Just { moduleOrigin = Just "Set", name = "union" }

                "diff" ->
                    Just { moduleOrigin = Just "Set", name = "difference" }

                "intersect" ->
                    Just { moduleOrigin = Just "Set", name = "intersect" }

                _ ->
                    Nothing

        [ "Json", "Encode" ] ->
            case reference.name of
                "encode" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_encode" }

                "null" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_null" }

                "bool" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_bool" }

                "string" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_string" }

                "int" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_int" }

                "float" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_float" }

                "list" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_list" }

                "array" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_array" }

                "set" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_set" }

                "object" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_object" }

                "dict" ->
                    Just { moduleOrigin = Nothing, name = "JsonEncode_dict" }

                _ ->
                    Nothing

        [ "Json", "Decode" ] ->
            case reference.name of
                "Field" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_Field" }

                "Index" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_Index" }

                "OneOf" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_OneOf" }

                "Failure" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_Failure" }

                "string" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_string" }

                "bool" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_bool" }

                "int" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_int" }

                "float" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_float" }

                "nullable" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_nullable" }

                "list" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_list" }

                "array" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_array" }

                "dict" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_dict" }

                "keyValuePairs" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_keyValuePairs" }

                "oneOrMore" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_oneOrMore" }

                "field" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_field" }

                "at" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_at" }

                "index" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_index" }

                "maybe" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_maybe" }

                "oneOf" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_oneOf" }

                "decodeString" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_decodeString" }

                "decodeValue" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_decodeValue" }

                "errorToString" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_errorToString" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_map" }

                "map2" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_map2" }

                "map3" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_map3" }

                "map4" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_map4" }

                "map5" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_map5" }

                "map6" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_map6" }

                "map7" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_map7" }

                "map8" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_map8" }

                "lazy" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_lazy" }

                "value" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_value" }

                "null" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_null" }

                "succeed" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_succeed" }

                "fail" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_fail" }

                "andThen" ->
                    Just { moduleOrigin = Nothing, name = "JsonDecode_andThen" }

                _ ->
                    Nothing

        [ "Regex" ] ->
            case reference.name of
                "fromString" ->
                    Just { moduleOrigin = Nothing, name = "Regex_fromString" }

                "fromStringWith" ->
                    Just { moduleOrigin = Nothing, name = "Regex_fromStringWith" }

                "never" ->
                    Just { moduleOrigin = Nothing, name = "Regex_never" }

                "contains" ->
                    Just { moduleOrigin = Nothing, name = "Regex_contains" }

                "split" ->
                    Just { moduleOrigin = Nothing, name = "Regex_split" }

                "find" ->
                    Just { moduleOrigin = Nothing, name = "Regex_find" }

                "replace" ->
                    Just { moduleOrigin = Nothing, name = "Regex_replace" }

                "splitAtMost" ->
                    Just { moduleOrigin = Nothing, name = "Regex_splitAtMost" }

                "findAtMost" ->
                    Just { moduleOrigin = Nothing, name = "Regex_findAtMost" }

                "replaceAtMost" ->
                    Just { moduleOrigin = Nothing, name = "Regex_replaceAtMost" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Nothing" ->
                    Just { moduleOrigin = Nothing, name = "None" }

                "Just" ->
                    Just { moduleOrigin = Nothing, name = "Some" }

                _ ->
                    Nothing

        [ "Result" ] ->
            case reference.name of
                "Err" ->
                    Just { moduleOrigin = Nothing, name = "Error" }

                "Ok" ->
                    Just { moduleOrigin = Nothing, name = "Ok" }

                _ ->
                    Nothing

        [ "Array" ] ->
            case reference.name of
                "isEmpty" ->
                    Just { moduleOrigin = Just "Array", name = "iEmpty" }

                "length" ->
                    Just { moduleOrigin = Nothing, name = "Array_length" }

                "get" ->
                    Just { moduleOrigin = Nothing, name = "Array_get" }

                "empty" ->
                    Just { moduleOrigin = Just "Array", name = "empty" }

                "initialize" ->
                    Just { moduleOrigin = Nothing, name = "Array_initialize" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "Array_repeat" }

                "fromList" ->
                    Just { moduleOrigin = Just "Array", name = "ofList" }

                "reverse" ->
                    Just { moduleOrigin = Just "Array", name = "rev" }

                "filter" ->
                    Just { moduleOrigin = Just "Array", name = "filter" }

                "push" ->
                    Just { moduleOrigin = Nothing, name = "Array_push" }

                "set" ->
                    Just { moduleOrigin = Nothing, name = "Array_set" }

                "slice" ->
                    Just { moduleOrigin = Nothing, name = "Array_slice" }

                "map" ->
                    Just { moduleOrigin = Just "Array", name = "map" }

                "indexedMap" ->
                    Just { moduleOrigin = Nothing, name = "Array_indexedMap" }

                "append" ->
                    Just { moduleOrigin = Just "Array", name = "append" }

                "toList" ->
                    Just { moduleOrigin = Just "Array", name = "toList" }

                "toIndexedList" ->
                    Just { moduleOrigin = Nothing, name = "Array_toIndexedList" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "Array_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "Array_foldr" }

                _ ->
                    Nothing

        [ "Random" ] ->
            case reference.name of
                "int" ->
                    Just { moduleOrigin = Nothing, name = "Random_int" }

                "float" ->
                    Just { moduleOrigin = Nothing, name = "Random_float" }

                "uniform" ->
                    Just { moduleOrigin = Nothing, name = "Random_uniform" }

                "weighted" ->
                    Just { moduleOrigin = Nothing, name = "Random_weighted" }

                "constant" ->
                    Just { moduleOrigin = Nothing, name = "Random_constant" }

                "list" ->
                    Just { moduleOrigin = Nothing, name = "Random_list" }

                "pair" ->
                    Just { moduleOrigin = Nothing, name = "Random_pair" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "Random_map" }

                "map2" ->
                    Just { moduleOrigin = Nothing, name = "Random_map2" }

                "map3" ->
                    Just { moduleOrigin = Nothing, name = "Random_map3" }

                "map4" ->
                    Just { moduleOrigin = Nothing, name = "Random_map4" }

                "map5" ->
                    Just { moduleOrigin = Nothing, name = "Random_map5" }

                "andThen" ->
                    Just { moduleOrigin = Nothing, name = "Random_andThen" }

                "lazy" ->
                    Just { moduleOrigin = Nothing, name = "Random_lazy" }

                "minInt" ->
                    Just { moduleOrigin = Nothing, name = "Random_minInt" }

                "maxInt" ->
                    Just { moduleOrigin = Nothing, name = "Random_maxInt" }

                "step" ->
                    Just { moduleOrigin = Nothing, name = "Random_step" }

                "initialSeed" ->
                    Just { moduleOrigin = Nothing, name = "Random_initialSeed" }

                "independentSeed" ->
                    Just { moduleOrigin = Nothing, name = "Random_independentSeed" }

                _ ->
                    Nothing

        [ "Time" ] ->
            case reference.name of
                "posixToMillis" ->
                    Just { moduleOrigin = Nothing, name = "Time_posixToMillis" }

                "millisToPosix" ->
                    Just { moduleOrigin = Nothing, name = "Time_millisToPosix" }

                "utc" ->
                    Just { moduleOrigin = Nothing, name = "Time_utc" }

                "toYear" ->
                    Just { moduleOrigin = Nothing, name = "Time_toYear" }

                "toMonth" ->
                    Just { moduleOrigin = Nothing, name = "Time_toMonth" }

                "toDay" ->
                    Just { moduleOrigin = Nothing, name = "Time_toDay" }

                "toWeekday" ->
                    Just { moduleOrigin = Nothing, name = "Time_toWeekday" }

                "toHour" ->
                    Just { moduleOrigin = Nothing, name = "Time_toHour" }

                "toMinute" ->
                    Just { moduleOrigin = Nothing, name = "Time_toMinute" }

                "toSecond" ->
                    Just { moduleOrigin = Nothing, name = "Time_toSecond" }

                "toMillis" ->
                    Just { moduleOrigin = Nothing, name = "Time_toMillis" }

                "customZone" ->
                    Just { moduleOrigin = Nothing, name = "Time_customZone" }

                _ ->
                    Nothing

        [ "Bytes" ] ->
            case reference.name of
                "LE" ->
                    Just { moduleOrigin = Nothing, name = "Bytes_LE" }

                "BE" ->
                    Just { moduleOrigin = Nothing, name = "Bytes_BE" }

                "width" ->
                    Just { moduleOrigin = Nothing, name = "Bytes_width" }

                _ ->
                    Nothing

        [ "Bytes", "Decode" ] ->
            case reference.name of
                "Loop" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_Loop" }

                "Done" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_Done" }

                "decode" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_decode" }

                "signedInt8" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_signedInt8" }

                "signedInt16" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_signedInt16" }

                "signedInt32" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_signedInt32" }

                "unsignedInt8" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_unsignedInt8" }

                "unsignedInt16" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_unsignedInt16" }

                "unsignedInt32" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_unsignedInt32" }

                "float32" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_float32" }

                "float64" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_float64" }

                "string" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_string" }

                "bytes" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_bytes" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_map" }

                "map2" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_map2" }

                "map3" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_map3" }

                "map4" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_map4" }

                "map5" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_map5" }

                "andThen" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_andThen" }

                "succeed" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_succeed" }

                "fail" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_fail" }

                "loop" ->
                    Just { moduleOrigin = Nothing, name = "BytesDecode_loop" }

                _ ->
                    Nothing

        [ "Bytes", "Encode" ] ->
            case reference.name of
                "encode" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_encode" }

                "signedInt8" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_signedInt8" }

                "signedInt16" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_signedInt16" }

                "signedInt32" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_signedInt32" }

                "unsignedInt8" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_unsignedInt8" }

                "unsignedInt16" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_unsignedInt16" }

                "unsignedInt32" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_unsignedInt32" }

                "float32" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_float32" }

                "float64" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_float64" }

                "bytes" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_bytes" }

                "string" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_string" }

                "getStringWidth" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_getStringWidth" }

                "sequence" ->
                    Just { moduleOrigin = Nothing, name = "BytesEncode_sequence" }

                _ ->
                    Nothing

        [ "Debug" ] ->
            case reference.name of
                "log" ->
                    Just { moduleOrigin = Nothing, name = "Debug_log" }

                "toString" ->
                    Just { moduleOrigin = Nothing, name = "Debug_toString" }

                "todo" ->
                    Just { moduleOrigin = Nothing, name = "Debug_todo" }

                _ ->
                    Nothing

        [ "Platform" ] ->
            case reference.name of
                "worker" ->
                    Just { moduleOrigin = Nothing, name = "Platform_worker" }

                _ ->
                    Nothing

        [ "Platform", "Cmd" ] ->
            case reference.name of
                "none" ->
                    Just { moduleOrigin = Nothing, name = "PlatformCmd_none" }

                "batch" ->
                    Just { moduleOrigin = Nothing, name = "PlatformCmd_batch" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "PlatformCmd_map" }

                _ ->
                    Nothing

        [ "Platform", "Sub" ] ->
            case reference.name of
                "none" ->
                    Just { moduleOrigin = Nothing, name = "PlatformSub_none" }

                "batch" ->
                    Just { moduleOrigin = Nothing, name = "PlatformSub_batch" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "PlatformSub_map" }

                _ ->
                    Nothing

        _ ->
            Nothing


referenceToFsharpName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
referenceToFsharpName reference =
    (reference.moduleOrigin |> String.concat)
        ++ "_"
        ++ reference.name


printFsharpPatternNotParenthesized : FsharpPattern -> Print
printFsharpPatternNotParenthesized fsharpPattern =
    -- IGNORE TCO
    case fsharpPattern of
        FsharpPatternIgnore ->
            printExactlyUnderscore

        FsharpPatternInt int ->
            Print.exactly (int |> intLiteral)

        FsharpPatternChar charValue ->
            Print.exactly (charLiteral charValue)

        FsharpPatternString string ->
            printFsharpString string

        FsharpPatternVariable name ->
            Print.exactly name

        FsharpPatternListCons fsharpPatternListCons ->
            printFsharpPatternListCons fsharpPatternListCons

        FsharpPatternListExact elements ->
            printFsharpPatternListExact elements

        FsharpPatternRecordInexhaustive recordInexhaustiveFieldNames ->
            Print.exactly "{ "
                |> Print.followedBy
                    (recordInexhaustiveFieldNames
                        |> FastDict.toList
                        |> Print.listMapAndIntersperseAndFlatten
                            (\( fieldName, fieldValuePattern ) ->
                                Print.exactly
                                    (fieldName
                                        ++ " = "
                                    )
                                    |> Print.followedBy
                                        (printFsharpPatternNotParenthesized
                                            fieldValuePattern
                                        )
                            )
                            (Print.exactly "; ")
                    )
                |> Print.followedBy (Print.exactly " }")

        FsharpPatternVariant patternVariant ->
            Print.exactly
                (qualifiedReferenceToFsharpName
                    { moduleOrigin = patternVariant.moduleOrigin
                    , name = patternVariant.name
                    }
                )
                |> Print.followedBy
                    (case patternVariant.values of
                        [] ->
                            Print.empty

                        variantValue0 :: variantValue1Up ->
                            Print.exactly "("
                                |> Print.followedBy
                                    ((variantValue0 :: variantValue1Up)
                                        |> Print.listMapAndIntersperseAndFlatten
                                            printFsharpPatternNotParenthesized
                                            (Print.exactly ", ")
                                    )
                                |> Print.followedBy (Print.exactly ")")
                    )

        FsharpPatternAs patternAs ->
            printFsharpPatternAs patternAs

        FsharpPatternTuple parts ->
            Print.exactly "(struct( "
                |> Print.followedBy
                    ((parts.part0 :: parts.part1 :: parts.part2Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            printFsharpPatternNotParenthesized
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly " ))")


printFsharpPatternListExact : List FsharpPattern -> Print
printFsharpPatternListExact elements =
    case elements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            Print.exactly "[ "
                |> Print.followedBy
                    ((element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\elementNode ->
                                Print.withIndentIncreasedBy 2
                                    (printFsharpPatternNotParenthesized elementNode)
                            )
                            (Print.exactly "; ")
                    )
                |> Print.followedBy (Print.exactly " ]")


printFsharpPatternAs :
    { variable : String
    , pattern : FsharpPattern
    }
    -> Print
printFsharpPatternAs syntaxAs =
    printFsharpPatternParenthesizedIfSpaceSeparated
        syntaxAs.pattern
        |> Print.followedBy
            (Print.exactly (" as " ++ syntaxAs.variable))


printFsharpExpressionRecord : FastDict.Dict String FsharpExpression -> Print
printFsharpExpressionRecord syntaxRecordFields =
    if syntaxRecordFields |> FastDict.isEmpty then
        Print.exactly "{}"

    else
        let
            fieldsPrint : Print
            fieldsPrint =
                syntaxRecordFields
                    |> FastDict.toList
                    |> Print.listMapAndIntersperseAndFlatten
                        (\( fieldName, fieldValue ) ->
                            let
                                fieldValuePrint : Print
                                fieldValuePrint =
                                    printFsharpExpressionNotParenthesized fieldValue
                            in
                            Print.withIndentIncreasedBy 2
                                (Print.exactly (fieldName ++ " =")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented
                                                (fieldValuePrint |> Print.lineSpread)
                                                |> Print.followedBy
                                                    fieldValuePrint
                                            )
                                        )
                                )
                        )
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (Print.exactly "; ")
                        )
        in
        Print.exactly "{ "
            |> Print.followedBy fieldsPrint
            |> Print.followedBy
                (Print.spaceOrLinebreakIndented
                    (fieldsPrint |> Print.lineSpread)
                )
            |> Print.followedBy (Print.exactly "}")


printParenthesized : { opening : String, closing : String, inner : Print } -> Print
printParenthesized config =
    Print.exactly config.opening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                config.inner
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented
                (config.inner |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly config.closing)


{-| Transpile a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
across multiple modules to value, function and type declarations.
Declarations that use unsupported stuff like html kernel code (directly or indirectly)
will not be present in the final declarations.
Their errors can be found alongside the valid transpiled declarations.

The given list of files must also include files from used dependencies
including `elm/core`.

-}
modules :
    List Elm.Syntax.File.File
    ->
        { errors : List String
        , declarations :
            { valuesAndFunctions :
                FastDict.Dict
                    String
                    { result : FsharpExpression
                    , type_ : FsharpType
                    }
            , typeAliases :
                FastDict.Dict
                    String
                    { parameters : List String
                    , type_ : FsharpType
                    }
            , recordTypes : FastSet.Set (List String)
            , choiceTypes :
                FastDict.Dict
                    String
                    { parameters : List String
                    , variants : FastDict.Dict String (Maybe FsharpType)
                    }
            }
        }
modules syntaxDeclarationsIncludingOverwrittenOnes =
    let
        syntaxModules : List Elm.Syntax.File.File
        syntaxModules =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Basics" ] ->
                                False

                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Char" ] ->
                                False

                            [ "String" ] ->
                                False

                            [ "List" ] ->
                                False

                            [ "Dict" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )

        moduleMembers :
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { valueOrFunctionOrTypeAliasNames : FastSet.Set String
                , choiceTypesExposingVariants :
                    FastDict.Dict String (FastDict.Dict String { valueCount : Int })
                , recordTypeAliases :
                    FastDict.Dict String (List String)
                , portsIncoming : FastSet.Set String
                , portsOutgoing : FastSet.Set String
                }
        moduleMembers =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        -- remove those modules we don't have a replacement for, yet
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )
                |> List.foldl
                    (\syntaxModule acrossModulesSoFar ->
                        acrossModulesSoFar
                            |> FastDict.insert
                                (syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                                )
                                (syntaxModule.declarations
                                    |> List.foldl
                                        (\(Elm.Syntax.Node.Node _ declaration) membersSoFar ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (syntaxValueOrFunctionDeclaration.declaration
                                                                    |> Elm.Syntax.Node.value
                                                                    |> .name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                    , recordTypeAliases =
                                                        membersSoFar.recordTypeAliases
                                                    , portsOutgoing = membersSoFar.portsOutgoing
                                                    , portsIncoming = membersSoFar.portsIncoming
                                                    }

                                                Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                            |> FastDict.insert
                                                                (syntaxChoiceTypeDeclaration.name |> Elm.Syntax.Node.value)
                                                                (syntaxChoiceTypeDeclaration.constructors
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ variant) variantNamesSoFar ->
                                                                            variantNamesSoFar
                                                                                |> FastDict.insert
                                                                                    (variant.name
                                                                                        |> Elm.Syntax.Node.value
                                                                                    )
                                                                                    { valueCount =
                                                                                        variant.arguments |> List.length
                                                                                    }
                                                                        )
                                                                        FastDict.empty
                                                                )
                                                    , recordTypeAliases =
                                                        membersSoFar.recordTypeAliases
                                                    , portsOutgoing = membersSoFar.portsOutgoing
                                                    , portsIncoming = membersSoFar.portsIncoming
                                                    }

                                                Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
                                                    let
                                                        typeAliasName : String
                                                        typeAliasName =
                                                            typeAlias.name
                                                                |> Elm.Syntax.Node.value
                                                    in
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert typeAliasName
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                    , recordTypeAliases =
                                                        case typeAlias.typeAnnotation |> Elm.Syntax.Node.value of
                                                            Elm.Syntax.TypeAnnotation.Record fields ->
                                                                membersSoFar.recordTypeAliases
                                                                    |> FastDict.insert typeAliasName
                                                                        (fields
                                                                            |> List.map
                                                                                (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, _ )) ->
                                                                                    fieldName |> stringFirstCharToUpper
                                                                                )
                                                                        )

                                                            _ ->
                                                                membersSoFar.recordTypeAliases
                                                    , portsOutgoing = membersSoFar.portsOutgoing
                                                    , portsIncoming = membersSoFar.portsIncoming
                                                    }

                                                Elm.Syntax.Declaration.PortDeclaration portDeclaration ->
                                                    if portDeclaration.typeAnnotation |> portTypeSignifiesOutgoing then
                                                        { valueOrFunctionOrTypeAliasNames =
                                                            membersSoFar.valueOrFunctionOrTypeAliasNames
                                                        , choiceTypesExposingVariants =
                                                            membersSoFar.choiceTypesExposingVariants
                                                        , recordTypeAliases =
                                                            membersSoFar.recordTypeAliases
                                                        , portsOutgoing =
                                                            membersSoFar.portsOutgoing
                                                                |> FastSet.insert
                                                                    (portDeclaration.name |> Elm.Syntax.Node.value)
                                                        , portsIncoming = membersSoFar.portsIncoming
                                                        }

                                                    else
                                                        { valueOrFunctionOrTypeAliasNames =
                                                            membersSoFar.valueOrFunctionOrTypeAliasNames
                                                        , choiceTypesExposingVariants =
                                                            membersSoFar.choiceTypesExposingVariants
                                                        , recordTypeAliases =
                                                            membersSoFar.recordTypeAliases
                                                        , portsOutgoing = membersSoFar.portsOutgoing
                                                        , portsIncoming =
                                                            membersSoFar.portsIncoming
                                                                |> FastSet.insert
                                                                    (portDeclaration.name |> Elm.Syntax.Node.value)
                                                        }

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    -- invalid syntax
                                                    membersSoFar
                                        )
                                        { valueOrFunctionOrTypeAliasNames = FastSet.empty
                                        , choiceTypesExposingVariants = FastDict.empty
                                        , recordTypeAliases = FastDict.empty
                                        , portsOutgoing = FastSet.empty
                                        , portsIncoming = FastSet.empty
                                        }
                                )
                    )
                    FastDict.empty

        syntaxModulesFromMostToLeastImported : List Elm.Syntax.File.File
        syntaxModulesFromMostToLeastImported =
            syntaxModules
                |> List.map
                    (\syntaxModule ->
                        ( syntaxModule
                        , syntaxModule.moduleDefinition
                            |> Elm.Syntax.Node.value
                            |> moduleHeaderName
                        , syntaxModule.imports
                            |> List.map
                                (\(Elm.Syntax.Node.Node _ import_) ->
                                    import_.moduleName |> Elm.Syntax.Node.value
                                )
                        )
                    )
                |> Data.Graph.stronglyConnComp
                |> -- we assume the given module do not have cyclic imports
                   Data.Graph.flattenSCCs

        syntaxModuleTypes :
            { errors : List String
            , types : FastDict.Dict Elm.Syntax.ModuleName.ModuleName ElmSyntaxTypeInfer.ModuleTypes
            }
        syntaxModuleTypes =
            syntaxModulesFromMostToLeastImported
                |> List.foldl
                    (\syntaxModule soFar ->
                        let
                            declarationTypesAndErrors : { types : ElmSyntaxTypeInfer.ModuleTypes, errors : List String }
                            declarationTypesAndErrors =
                                syntaxModule.declarations
                                    |> List.map Elm.Syntax.Node.value
                                    |> ElmSyntaxTypeInfer.moduleDeclarationsToTypes
                                        (syntaxModule.imports
                                            |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                                                soFar.types
                                        )
                        in
                        { errors = declarationTypesAndErrors.errors ++ soFar.errors
                        , types =
                            soFar.types
                                |> FastDict.insert
                                    (syntaxModule.moduleDefinition
                                        |> Elm.Syntax.Node.value
                                        |> moduleHeaderName
                                    )
                                    declarationTypesAndErrors.types
                        }
                    )
                    { errors = []
                    , types =
                        ElmSyntaxTypeInfer.elmCoreTypes
                            |> FastDict.union elmJsonTypes
                            |> FastDict.union elmBytesTypes
                            |> FastDict.union elmRegexTypes
                            |> FastDict.union elmKernelParserTypes
                            |> FastDict.union elmKernelUrlTypes
                    }

        syntaxModulesInferred :
            Result
                String
                (List
                    { module_ : Elm.Syntax.File.File
                    , declarationsInferred :
                        FastDict.Dict
                            String
                            { nameRange : Elm.Syntax.Range.Range
                            , documentation : Maybe { content : String, range : Elm.Syntax.Range.Range }
                            , signature :
                                Maybe
                                    { range : Elm.Syntax.Range.Range
                                    , nameRange : Elm.Syntax.Range.Range
                                    , annotationType : Elm.Syntax.TypeAnnotation.TypeAnnotation
                                    , annotationTypeRange : Elm.Syntax.Range.Range
                                    }
                            , parameters : List (ElmSyntaxTypeInfer.TypedNode (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String)) (ElmSyntaxTypeInfer.Type String))
                            , result : ElmSyntaxTypeInfer.TypedNode (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type String)) (ElmSyntaxTypeInfer.Type String)
                            , type_ : ElmSyntaxTypeInfer.Type String
                            }
                    }
                )
        syntaxModulesInferred =
            syntaxModules
                |> listMapAndCombineOk
                    (\syntaxModule ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                        in
                        -- _ =
                        --     Debug.log "inferring module" (moduleName |> String.join ".")
                        case syntaxModuleTypes.types |> FastDict.get moduleName of
                            Nothing ->
                                Err "module types did not contain info about the module name"

                            Just otherModuleDeclaredTypes ->
                                syntaxModule.declarations
                                    |> List.filterMap
                                        (\(Elm.Syntax.Node.Node _ declaration) ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    Just syntaxValueOrFunctionDeclaration

                                                Elm.Syntax.Declaration.AliasDeclaration _ ->
                                                    Nothing

                                                Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                                                    Nothing

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    Nothing

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    Nothing

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    Nothing
                                        )
                                    |> ElmSyntaxTypeInfer.valueAndFunctionDeclarations
                                        { importedTypes =
                                            syntaxModuleTypes.types
                                                |> FastDict.remove moduleName
                                        , moduleOriginLookup =
                                            syntaxModule.imports
                                                |> ElmSyntaxTypeInfer.importsToModuleOriginLookup
                                                    syntaxModuleTypes.types
                                        , otherModuleDeclaredTypes =
                                            { signatures = FastDict.empty
                                            , typeAliases = otherModuleDeclaredTypes.typeAliases
                                            , choiceTypes = otherModuleDeclaredTypes.choiceTypes
                                            }
                                        }
                                    |> Result.map
                                        (\declarationsInferred ->
                                            { declarationsInferred = declarationsInferred
                                            , module_ = syntaxModule
                                            }
                                        )
                                    |> Result.mapError
                                        (\error ->
                                            "In module "
                                                ++ (moduleName |> String.join ".")
                                                ++ " "
                                                ++ error
                                        )
                    )
    in
    case syntaxModulesInferred of
        Err error ->
            { errors = error :: syntaxModuleTypes.errors
            , declarations =
                { valuesAndFunctions = FastDict.empty
                , typeAliases = FastDict.empty
                , choiceTypes = FastDict.empty
                , recordTypes = FastSet.empty
                }
            }

        Ok modulesInferred ->
            let
                fsharpDeclarationsWithoutExtraRecordTypeAliases :
                    { errors : List String
                    , declarations :
                        { valuesAndFunctions :
                            FastDict.Dict
                                String
                                { result : FsharpExpression
                                , type_ : FsharpType
                                }
                        , typeAliases :
                            FastDict.Dict
                                String
                                { parameters : List String
                                , type_ : FsharpType
                                }
                        , choiceTypes :
                            FastDict.Dict
                                String
                                { parameters : List String
                                , variants : FastDict.Dict String (Maybe FsharpType)
                                }
                        }
                    }
                fsharpDeclarationsWithoutExtraRecordTypeAliases =
                    modulesInferred
                        |> List.foldr
                            (\moduleInferred soFarAcrossModules ->
                                let
                                    moduleName : Elm.Syntax.ModuleName.ModuleName
                                    moduleName =
                                        moduleInferred.module_.moduleDefinition
                                            |> Elm.Syntax.Node.value
                                            |> moduleHeaderName

                                    createdModuleContext : ModuleContext
                                    createdModuleContext =
                                        moduleContextMerge
                                            (moduleInferred.module_.imports |> importsToModuleContext moduleMembers)
                                            (case moduleMembers |> FastDict.get moduleName of
                                                Nothing ->
                                                    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                                        FastDict.empty
                                                    , variantLookup = FastDict.empty
                                                    , recordTypeAliasLookup = FastDict.empty
                                                    , portIncomingLookup = FastSet.empty
                                                    , portOutgoingLookup = FastSet.empty
                                                    }

                                                Just moduleLocalNames ->
                                                    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                                        FastSet.union
                                                            moduleLocalNames.valueOrFunctionOrTypeAliasNames
                                                            (moduleLocalNames.choiceTypesExposingVariants
                                                                |> FastDict.foldl
                                                                    (\choiceTypeName _ soFar ->
                                                                        soFar |> FastSet.insert choiceTypeName
                                                                    )
                                                                    FastSet.empty
                                                            )
                                                            |> FastSet.foldl
                                                                (\name soFar ->
                                                                    soFar
                                                                        |> FastDict.insert ( [], name )
                                                                            moduleName
                                                                )
                                                                FastDict.empty
                                                    , variantLookup =
                                                        moduleLocalNames.choiceTypesExposingVariants
                                                            |> FastDict.foldl
                                                                (\_ variantNames soFarAcrossChoiceTypes ->
                                                                    variantNames
                                                                        |> FastDict.foldl
                                                                            (\name info soFar ->
                                                                                soFar
                                                                                    |> FastDict.insert ( [], name )
                                                                                        { moduleOrigin = moduleName
                                                                                        , valueCount = info.valueCount
                                                                                        }
                                                                            )
                                                                            soFarAcrossChoiceTypes
                                                                )
                                                                FastDict.empty
                                                    , recordTypeAliasLookup =
                                                        moduleLocalNames.recordTypeAliases
                                                            |> FastDict.foldl
                                                                (\name fieldOrder soFar ->
                                                                    soFar
                                                                        |> FastDict.insert ( [], name )
                                                                            fieldOrder
                                                                )
                                                                FastDict.empty
                                                    , portIncomingLookup =
                                                        moduleLocalNames.portsIncoming
                                                            |> FastSet.map (\portName -> ( [], portName ))
                                                    , portOutgoingLookup =
                                                        moduleLocalNames.portsOutgoing
                                                            |> FastSet.map (\portName -> ( [], portName ))
                                                    }
                                            )
                                in
                                moduleInferred.module_.declarations
                                    |> List.foldr
                                        (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    let
                                                        declarationName : String
                                                        declarationName =
                                                            syntaxValueOrFunctionDeclaration.declaration
                                                                |> Elm.Syntax.Node.value
                                                                |> .name
                                                                |> Elm.Syntax.Node.value
                                                    in
                                                    case moduleInferred.declarationsInferred |> FastDict.get declarationName of
                                                        Just valueOrFunctionDeclarationInferred ->
                                                            case
                                                                valueOrFunctionDeclarationInferred
                                                                    |> valueOrFunctionDeclarationSetLocalToOrigin moduleName
                                                                    |> valueOrFunctionDeclaration createdModuleContext
                                                            of
                                                                Ok fsharpValueOrFunctionDeclaration ->
                                                                    { errors = soFar.errors
                                                                    , declarations =
                                                                        { typeAliases = soFar.declarations.typeAliases
                                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                                        , valuesAndFunctions =
                                                                            soFar.declarations.valuesAndFunctions
                                                                                |> FastDict.insert
                                                                                    ({ moduleOrigin = moduleName
                                                                                     , name = declarationName
                                                                                     }
                                                                                        |> referenceToFsharpName
                                                                                    )
                                                                                    fsharpValueOrFunctionDeclaration
                                                                        }
                                                                    }

                                                                Err error ->
                                                                    { declarations = soFar.declarations
                                                                    , errors = error :: soFar.errors
                                                                    }

                                                        Nothing ->
                                                            { declarations = soFar.declarations
                                                            , errors = (declarationName ++ " not inferred") :: soFar.errors
                                                            }

                                                Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
                                                    case syntaxTypeAliasDeclaration |> typeAliasDeclaration createdModuleContext of
                                                        Ok fsharpTypeAliasDeclaration ->
                                                            { errors = soFar.errors
                                                            , declarations =
                                                                { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                                , choiceTypes = soFar.declarations.choiceTypes
                                                                , typeAliases =
                                                                    soFar.declarations.typeAliases
                                                                        |> FastDict.insert
                                                                            ({ moduleOrigin = moduleName
                                                                             , name = fsharpTypeAliasDeclaration.name
                                                                             }
                                                                                |> referenceToFsharpName
                                                                            )
                                                                            { parameters = fsharpTypeAliasDeclaration.parameters
                                                                            , type_ = fsharpTypeAliasDeclaration.type_
                                                                            }
                                                                }
                                                            }

                                                        Err error ->
                                                            { declarations = soFar.declarations
                                                            , errors = error :: soFar.errors
                                                            }

                                                Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
                                                    case syntaxChoiceTypeDeclaration.name |> Elm.Syntax.Node.value of
                                                        "Maybe" ->
                                                            soFar

                                                        "Result" ->
                                                            soFar

                                                        _ ->
                                                            case syntaxChoiceTypeDeclaration |> choiceTypeDeclaration createdModuleContext of
                                                                Ok fsharpTypeAliasDeclaration ->
                                                                    { errors = soFar.errors
                                                                    , declarations =
                                                                        { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                                        , typeAliases = soFar.declarations.typeAliases
                                                                        , choiceTypes =
                                                                            soFar.declarations.choiceTypes
                                                                                |> FastDict.insert
                                                                                    ({ moduleOrigin = moduleName
                                                                                     , name = fsharpTypeAliasDeclaration.name
                                                                                     }
                                                                                        |> referenceToFsharpName
                                                                                    )
                                                                                    { parameters = fsharpTypeAliasDeclaration.parameters
                                                                                    , variants =
                                                                                        fsharpTypeAliasDeclaration.variants
                                                                                            |> FastDict.foldl
                                                                                                (\variantName maybeValue variantsSoFar ->
                                                                                                    variantsSoFar
                                                                                                        |> FastDict.insert
                                                                                                            ({ moduleOrigin = moduleName
                                                                                                             , name = variantName
                                                                                                             }
                                                                                                                |> referenceToFsharpName
                                                                                                            )
                                                                                                            maybeValue
                                                                                                )
                                                                                                FastDict.empty
                                                                                    }
                                                                        }
                                                                    }

                                                                Err error ->
                                                                    { declarations = soFar.declarations
                                                                    , errors = error :: soFar.errors
                                                                    }

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    soFar

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    soFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    soFar
                                        )
                                        soFarAcrossModules
                            )
                            { errors = []
                            , declarations =
                                { valuesAndFunctions = FastDict.empty
                                , typeAliases = FastDict.empty
                                , choiceTypes = FastDict.empty
                                }
                            }

                additionalRecordTypeAliases : FastSet.Set (List String)
                additionalRecordTypeAliases =
                    FastSet.union
                        (fsharpDeclarationsWithoutExtraRecordTypeAliases.declarations.valuesAndFunctions
                            |> fastDictMapToFastSetAndUnify
                                (\valueOrFunctionInfo ->
                                    FastSet.union
                                        (valueOrFunctionInfo.result
                                            |> fsharpExpressionContainedConstructedRecords
                                        )
                                        (valueOrFunctionInfo.type_
                                            |> fsharpTypeContainedRecords
                                        )
                                )
                        )
                        (FastSet.union
                            (fsharpDeclarationsWithoutExtraRecordTypeAliases.declarations.typeAliases
                                |> fastDictMapToFastSetAndUnify
                                    (\typeAliasInfo ->
                                        typeAliasInfo.type_
                                            |> fsharpTypeContainedRecords
                                    )
                            )
                            (fsharpDeclarationsWithoutExtraRecordTypeAliases.declarations.choiceTypes
                                |> fastDictMapToFastSetAndUnify
                                    (\choiceTypeInfo ->
                                        choiceTypeInfo.variants
                                            |> fastDictMapToFastSetAndUnify
                                                (\maybeValue ->
                                                    case maybeValue of
                                                        Nothing ->
                                                            FastSet.empty

                                                        Just variantValue ->
                                                            variantValue
                                                                |> fsharpTypeContainedRecords
                                                )
                                    )
                            )
                        )
            in
            { declarations =
                { valuesAndFunctions =
                    fsharpDeclarationsWithoutExtraRecordTypeAliases.declarations.valuesAndFunctions
                        |> FastDict.map
                            (\_ valueOrFunctionInfo ->
                                { type_ = valueOrFunctionInfo.type_
                                , result = valueOrFunctionInfo.result
                                }
                            )
                , choiceTypes =
                    fsharpDeclarationsWithoutExtraRecordTypeAliases.declarations.choiceTypes
                        |> FastDict.map
                            (\_ typeAliasInfo ->
                                { parameters = typeAliasInfo.parameters
                                , variants = typeAliasInfo.variants
                                }
                            )
                , recordTypes = additionalRecordTypeAliases
                , typeAliases =
                    fsharpDeclarationsWithoutExtraRecordTypeAliases.declarations.typeAliases
                        |> FastDict.map
                            (\_ typeAliasInfo ->
                                { parameters = typeAliasInfo.parameters
                                , type_ = typeAliasInfo.type_
                                }
                            )
                }
            , errors =
                syntaxModuleTypes.errors
                    ++ fsharpDeclarationsWithoutExtraRecordTypeAliases.errors
            }


portTypeSignifiesOutgoing :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Bool
portTypeSignifiesOutgoing (Elm.Syntax.Node.Node _ syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation _ (Elm.Syntax.Node.Node _ output) ->
            case output of
                Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( _, name )) _ ->
                    name |> String.toLower |> String.contains "cmd"

                _ ->
                    False

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( _, name )) _ ->
            name |> String.toLower |> String.contains "cmd"

        _ ->
            False


valueOrFunctionDeclarationSetLocalToOrigin :
    Elm.Syntax.ModuleName.ModuleName
    ->
        { nameRange : Elm.Syntax.Range.Range
        , documentation : Maybe { content : String, range : Elm.Syntax.Range.Range }
        , signature :
            Maybe
                { range : Elm.Syntax.Range.Range
                , nameRange : Elm.Syntax.Range.Range
                , annotationType : Elm.Syntax.TypeAnnotation.TypeAnnotation
                , annotationTypeRange : Elm.Syntax.Range.Range
                }
        , parameters :
            List
                (ElmSyntaxTypeInfer.TypedNode
                    (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
                    (ElmSyntaxTypeInfer.Type String)
                )
        , result :
            ElmSyntaxTypeInfer.TypedNode
                (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type String))
                (ElmSyntaxTypeInfer.Type String)
        , type_ : ElmSyntaxTypeInfer.Type String
        }
    ->
        { nameRange : Elm.Syntax.Range.Range
        , documentation : Maybe { content : String, range : Elm.Syntax.Range.Range }
        , signature :
            Maybe
                { range : Elm.Syntax.Range.Range
                , nameRange : Elm.Syntax.Range.Range
                , annotationType : Elm.Syntax.TypeAnnotation.TypeAnnotation
                , annotationTypeRange : Elm.Syntax.Range.Range
                }
        , parameters :
            List
                (ElmSyntaxTypeInfer.TypedNode
                    (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
                    (ElmSyntaxTypeInfer.Type String)
                )
        , result :
            ElmSyntaxTypeInfer.TypedNode
                (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type String))
                (ElmSyntaxTypeInfer.Type String)
        , type_ : ElmSyntaxTypeInfer.Type String
        }
valueOrFunctionDeclarationSetLocalToOrigin moduleOrigin inferredValueOrFunctionDeclaration =
    { nameRange = inferredValueOrFunctionDeclaration.nameRange
    , documentation = inferredValueOrFunctionDeclaration.documentation
    , signature = inferredValueOrFunctionDeclaration.signature
    , parameters =
        inferredValueOrFunctionDeclaration.parameters
            |> List.map
                (\parameter ->
                    parameter
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                )
    , result =
        inferredValueOrFunctionDeclaration.result
            |> expressionTypedNodeSetLocalToOrigin
                { localExpressionVariables =
                    inferredValueOrFunctionDeclaration.parameters
                        |> listMapToFastSetsAndUnify patternTypedNodeIntroducedVariables
                }
                moduleOrigin
    , type_ =
        inferredValueOrFunctionDeclaration.type_
            |> typeSetLocalToOrigin moduleOrigin
    }


patternTypedNodeSetLocalToOrigin :
    Elm.Syntax.ModuleName.ModuleName
    ->
        ElmSyntaxTypeInfer.TypedNode
            (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type comparableVariable))
            (ElmSyntaxTypeInfer.Type comparableVariable)
    ->
        ElmSyntaxTypeInfer.TypedNode
            (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type comparableVariable))
            (ElmSyntaxTypeInfer.Type comparableVariable)
patternTypedNodeSetLocalToOrigin moduleOrigin patternTypedNode =
    { range = patternTypedNode.range
    , type_ =
        patternTypedNode.type_
            |> typeSetLocalToOrigin moduleOrigin
    , value =
        patternTypedNode.value
            |> patternSetLocalToOrigin moduleOrigin
    }


patternSetLocalToOrigin :
    Elm.Syntax.ModuleName.ModuleName
    -> ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type comparableVariable)
    -> ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type comparableVariable)
patternSetLocalToOrigin moduleOrigin patternInferred =
    case patternInferred of
        ElmSyntaxTypeInfer.PatternIgnored ->
            ElmSyntaxTypeInfer.PatternIgnored

        ElmSyntaxTypeInfer.PatternUnit ->
            ElmSyntaxTypeInfer.PatternUnit

        ElmSyntaxTypeInfer.PatternChar char ->
            ElmSyntaxTypeInfer.PatternChar char

        ElmSyntaxTypeInfer.PatternString string ->
            ElmSyntaxTypeInfer.PatternString string

        ElmSyntaxTypeInfer.PatternInt int ->
            ElmSyntaxTypeInfer.PatternInt int

        ElmSyntaxTypeInfer.PatternVariable variable ->
            ElmSyntaxTypeInfer.PatternVariable variable

        ElmSyntaxTypeInfer.PatternParenthesized inParens ->
            ElmSyntaxTypeInfer.PatternParenthesized
                (inParens
                    |> patternTypedNodeSetLocalToOrigin moduleOrigin
                )

        ElmSyntaxTypeInfer.PatternAs patternAs ->
            ElmSyntaxTypeInfer.PatternAs
                { variable =
                    { range = patternAs.variable.range
                    , value = patternAs.variable.value
                    , type_ =
                        patternAs.variable.type_
                            |> typeSetLocalToOrigin moduleOrigin
                    }
                , pattern =
                    patternAs.pattern
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                }

        ElmSyntaxTypeInfer.PatternListCons listCons ->
            ElmSyntaxTypeInfer.PatternListCons
                { head =
                    listCons.head
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                , tail =
                    listCons.tail
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                }

        ElmSyntaxTypeInfer.PatternTuple parts ->
            ElmSyntaxTypeInfer.PatternTuple
                { part0 =
                    parts.part0
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                , part1 =
                    parts.part1
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                }

        ElmSyntaxTypeInfer.PatternTriple parts ->
            ElmSyntaxTypeInfer.PatternTriple
                { part0 =
                    parts.part0
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                , part1 =
                    parts.part1
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                , part2 =
                    parts.part2
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                }

        ElmSyntaxTypeInfer.PatternRecord fields ->
            ElmSyntaxTypeInfer.PatternRecord
                (fields
                    |> List.map
                        (\field ->
                            { range = field.range
                            , value = field.value
                            , type_ =
                                field.type_
                                    |> typeSetLocalToOrigin moduleOrigin
                            }
                        )
                )

        ElmSyntaxTypeInfer.PatternListExact elements ->
            ElmSyntaxTypeInfer.PatternListExact
                (elements
                    |> List.map
                        (\element ->
                            element
                                |> patternTypedNodeSetLocalToOrigin moduleOrigin
                        )
                )

        ElmSyntaxTypeInfer.PatternVariant patternVariant ->
            ElmSyntaxTypeInfer.PatternVariant
                { name = patternVariant.name
                , qualification = patternVariant.qualification
                , moduleOrigin =
                    case patternVariant.moduleOrigin of
                        moduleOriginPart0 :: moduleOriginPart1Up ->
                            moduleOriginPart0 :: moduleOriginPart1Up

                        [] ->
                            moduleOrigin
                , values =
                    patternVariant.values
                        |> List.map
                            (\value ->
                                value
                                    |> patternTypedNodeSetLocalToOrigin moduleOrigin
                            )
                }


expressionTypedNodeSetLocalToOrigin :
    { localExpressionVariables : FastSet.Set String }
    -> Elm.Syntax.ModuleName.ModuleName
    ->
        ElmSyntaxTypeInfer.TypedNode
            (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type comparableVariable))
            (ElmSyntaxTypeInfer.Type comparableVariable)
    ->
        ElmSyntaxTypeInfer.TypedNode
            (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type comparableVariable))
            (ElmSyntaxTypeInfer.Type comparableVariable)
expressionTypedNodeSetLocalToOrigin context moduleOrigin expressionTypedNode =
    { range = expressionTypedNode.range
    , type_ = expressionTypedNode.type_ |> typeSetLocalToOrigin moduleOrigin
    , value =
        expressionTypedNode.value
            |> expressionSetLocalToOrigin context moduleOrigin
    }


expressionSetLocalToOrigin :
    { localExpressionVariables : FastSet.Set String }
    -> Elm.Syntax.ModuleName.ModuleName
    -> ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type comparableVariable)
    -> ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type comparableVariable)
expressionSetLocalToOrigin context moduleOrigin inferredExpression =
    case inferredExpression of
        ElmSyntaxTypeInfer.ExpressionUnit ->
            ElmSyntaxTypeInfer.ExpressionUnit

        ElmSyntaxTypeInfer.ExpressionInteger integer ->
            ElmSyntaxTypeInfer.ExpressionInteger integer

        ElmSyntaxTypeInfer.ExpressionFloat float ->
            ElmSyntaxTypeInfer.ExpressionFloat float

        ElmSyntaxTypeInfer.ExpressionString string ->
            ElmSyntaxTypeInfer.ExpressionString string

        ElmSyntaxTypeInfer.ExpressionChar char ->
            ElmSyntaxTypeInfer.ExpressionChar char

        ElmSyntaxTypeInfer.ExpressionReference reference ->
            ElmSyntaxTypeInfer.ExpressionReference
                (case reference.moduleOrigin of
                    [] ->
                        if context.localExpressionVariables |> FastSet.member reference.name then
                            reference

                        else
                            { name = reference.name
                            , qualification = reference.qualification
                            , moduleOrigin = moduleOrigin
                            }

                    _ :: _ ->
                        reference
                )

        ElmSyntaxTypeInfer.ExpressionOperatorFunction operatorFunction ->
            ElmSyntaxTypeInfer.ExpressionOperatorFunction operatorFunction

        ElmSyntaxTypeInfer.ExpressionRecordAccessFunction recordAccessFunction ->
            ElmSyntaxTypeInfer.ExpressionRecordAccessFunction recordAccessFunction

        ElmSyntaxTypeInfer.ExpressionNegation negated ->
            ElmSyntaxTypeInfer.ExpressionNegation
                (negated
                    |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                )

        ElmSyntaxTypeInfer.ExpressionParenthesized inParens ->
            ElmSyntaxTypeInfer.ExpressionParenthesized
                (inParens
                    |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                )

        ElmSyntaxTypeInfer.ExpressionTuple parts ->
            ElmSyntaxTypeInfer.ExpressionTuple
                { part0 =
                    parts.part0
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , part1 =
                    parts.part1
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                }

        ElmSyntaxTypeInfer.ExpressionTriple parts ->
            ElmSyntaxTypeInfer.ExpressionTriple
                { part0 =
                    parts.part0
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , part1 =
                    parts.part1
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , part2 =
                    parts.part2
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                }

        ElmSyntaxTypeInfer.ExpressionRecordAccess recordAccess ->
            ElmSyntaxTypeInfer.ExpressionRecordAccess
                { record =
                    recordAccess.record
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , fieldNameRange = recordAccess.fieldNameRange
                , fieldName = recordAccess.fieldName
                }

        ElmSyntaxTypeInfer.ExpressionInfixOperation infixOperation ->
            ElmSyntaxTypeInfer.ExpressionInfixOperation
                { operator =
                    { symbol = infixOperation.operator.symbol
                    , moduleOrigin = infixOperation.operator.moduleOrigin
                    , type_ =
                        infixOperation.operator.type_
                            |> typeSetLocalToOrigin moduleOrigin
                    }
                , left =
                    infixOperation.left
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , right =
                    infixOperation.right
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                }

        ElmSyntaxTypeInfer.ExpressionIfThenElse ifThenElse ->
            ElmSyntaxTypeInfer.ExpressionIfThenElse
                { condition =
                    ifThenElse.condition
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , onTrue =
                    ifThenElse.onTrue
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , onFalse =
                    ifThenElse.onFalse
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                }

        ElmSyntaxTypeInfer.ExpressionList elements ->
            ElmSyntaxTypeInfer.ExpressionList
                (elements
                    |> List.map
                        (\element ->
                            element |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                        )
                )

        ElmSyntaxTypeInfer.ExpressionCall call ->
            ElmSyntaxTypeInfer.ExpressionCall
                { called =
                    call.called
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , argument0 =
                    call.argument0
                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                , argument1Up =
                    call.argument1Up
                        |> List.map
                            (\element ->
                                element |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                            )
                }

        ElmSyntaxTypeInfer.ExpressionRecord fields ->
            ElmSyntaxTypeInfer.ExpressionRecord
                (fields
                    |> List.map
                        (\field ->
                            { range = field.range
                            , nameRange = field.nameRange
                            , name = field.name
                            , value =
                                field.value
                                    |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                            }
                        )
                )

        ElmSyntaxTypeInfer.ExpressionRecordUpdate recordUpdate ->
            ElmSyntaxTypeInfer.ExpressionRecordUpdate
                { recordVariable =
                    { range = recordUpdate.recordVariable.range
                    , type_ =
                        recordUpdate.recordVariable.type_
                            |> typeSetLocalToOrigin moduleOrigin
                    , value =
                        recordUpdate.recordVariable.value
                            |> (\reference ->
                                    case reference.moduleOrigin of
                                        [] ->
                                            if context.localExpressionVariables |> FastSet.member reference.name then
                                                reference

                                            else
                                                { name = reference.name
                                                , moduleOrigin = moduleOrigin
                                                }

                                        _ :: _ ->
                                            reference
                               )
                    }
                , field0 =
                    recordUpdate.field0
                        |> (\field ->
                                { range = field.range
                                , nameRange = field.nameRange
                                , name = field.name
                                , value =
                                    field.value
                                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                                }
                           )
                , field1Up =
                    recordUpdate.field1Up
                        |> List.map
                            (\field ->
                                { range = field.range
                                , nameRange = field.nameRange
                                , name = field.name
                                , value =
                                    field.value
                                        |> expressionTypedNodeSetLocalToOrigin context moduleOrigin
                                }
                            )
                }

        ElmSyntaxTypeInfer.ExpressionLambda lambda ->
            let
                introducedParameterPatternVariables : FastSet.Set String
                introducedParameterPatternVariables =
                    (lambda.parameter0 :: lambda.parameter1Up)
                        |> listMapToFastSetsAndUnify
                            patternTypedNodeIntroducedVariables
            in
            ElmSyntaxTypeInfer.ExpressionLambda
                { parameter0 =
                    lambda.parameter0
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                , parameter1Up =
                    lambda.parameter1Up
                        |> List.map
                            (\parameter ->
                                parameter
                                    |> patternTypedNodeSetLocalToOrigin moduleOrigin
                            )
                , result =
                    lambda.result
                        |> expressionTypedNodeSetLocalToOrigin
                            { localExpressionVariables =
                                FastSet.union
                                    context.localExpressionVariables
                                    introducedParameterPatternVariables
                            }
                            moduleOrigin
                }

        ElmSyntaxTypeInfer.ExpressionLetIn letIn ->
            let
                introducedExpressionVariablesAcrossLetIn : FastSet.Set String
                introducedExpressionVariablesAcrossLetIn =
                    (letIn.declaration0 :: letIn.declaration1Up)
                        |> listMapToFastSetsAndUnify
                            (\inferredLetDeclaration ->
                                case inferredLetDeclaration.declaration of
                                    ElmSyntaxTypeInfer.LetDestructuring letDestructuring ->
                                        letDestructuring.pattern
                                            |> patternTypedNodeIntroducedVariables

                                    ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration letValueOrFunction ->
                                        FastSet.insert letValueOrFunction.name
                                            (letValueOrFunction.parameters
                                                |> listMapToFastSetsAndUnify
                                                    patternTypedNodeIntroducedVariables
                                            )
                            )

                newContext : { localExpressionVariables : FastSet.Set String }
                newContext =
                    { localExpressionVariables =
                        FastSet.union context.localExpressionVariables
                            introducedExpressionVariablesAcrossLetIn
                    }
            in
            ElmSyntaxTypeInfer.ExpressionLetIn
                { declaration0 =
                    letIn.declaration0
                        |> (\inferredLetDeclaration ->
                                { range = inferredLetDeclaration.range
                                , declaration =
                                    inferredLetDeclaration.declaration
                                        |> letDeclarationSetLocalToOrigin newContext
                                            moduleOrigin
                                }
                           )
                , declaration1Up =
                    letIn.declaration1Up
                        |> List.map
                            (\inferredLetDeclaration ->
                                { range = inferredLetDeclaration.range
                                , declaration =
                                    inferredLetDeclaration.declaration
                                        |> letDeclarationSetLocalToOrigin newContext
                                            moduleOrigin
                                }
                            )
                , result =
                    letIn.result
                        |> expressionTypedNodeSetLocalToOrigin newContext
                            moduleOrigin
                }

        ElmSyntaxTypeInfer.ExpressionCaseOf caseOf ->
            ElmSyntaxTypeInfer.ExpressionCaseOf
                { matchedExpression =
                    caseOf.matchedExpression
                        |> expressionTypedNodeSetLocalToOrigin
                            context
                            moduleOrigin
                , case0 =
                    caseOf.case0
                        |> (\caseInferred ->
                                { pattern =
                                    caseInferred.pattern
                                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                                , result =
                                    caseInferred.result
                                        |> expressionTypedNodeSetLocalToOrigin
                                            { localExpressionVariables =
                                                FastSet.union
                                                    context.localExpressionVariables
                                                    (caseInferred.pattern
                                                        |> patternTypedNodeIntroducedVariables
                                                    )
                                            }
                                            moduleOrigin
                                }
                           )
                , case1Up =
                    caseOf.case1Up
                        |> List.map
                            (\caseInferred ->
                                { pattern =
                                    caseInferred.pattern
                                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                                , result =
                                    caseInferred.result
                                        |> expressionTypedNodeSetLocalToOrigin
                                            { localExpressionVariables =
                                                FastSet.union
                                                    context.localExpressionVariables
                                                    (caseInferred.pattern
                                                        |> patternTypedNodeIntroducedVariables
                                                    )
                                            }
                                            moduleOrigin
                                }
                            )
                }


letDeclarationSetLocalToOrigin :
    { localExpressionVariables : FastSet.Set String }
    -> Elm.Syntax.ModuleName.ModuleName
    -> ElmSyntaxTypeInfer.LetDeclaration (ElmSyntaxTypeInfer.Type comparableVariable)
    -> ElmSyntaxTypeInfer.LetDeclaration (ElmSyntaxTypeInfer.Type comparableVariable)
letDeclarationSetLocalToOrigin context moduleOrigin inferredLetDeclaration =
    case inferredLetDeclaration of
        ElmSyntaxTypeInfer.LetDestructuring letDestructuring ->
            ElmSyntaxTypeInfer.LetDestructuring
                { pattern =
                    letDestructuring.pattern
                        |> patternTypedNodeSetLocalToOrigin moduleOrigin
                , expression =
                    letDestructuring.expression
                        |> expressionTypedNodeSetLocalToOrigin context
                            moduleOrigin
                }

        ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration letValueOrFunction ->
            ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration
                { signature = letValueOrFunction.signature
                , nameRange = letValueOrFunction.nameRange
                , name = letValueOrFunction.name
                , parameters =
                    letValueOrFunction.parameters
                        |> List.map
                            (\parameter ->
                                parameter
                                    |> patternTypedNodeSetLocalToOrigin moduleOrigin
                            )
                , result =
                    letValueOrFunction.result
                        |> expressionTypedNodeSetLocalToOrigin context
                            moduleOrigin
                , type_ = letValueOrFunction.type_ |> typeSetLocalToOrigin moduleOrigin
                }


patternTypedNodeIntroducedVariables :
    ElmSyntaxTypeInfer.TypedNode
        (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type comparableTypeVariable))
        (ElmSyntaxTypeInfer.Type comparableTypeVariable)
    -> FastSet.Set String
patternTypedNodeIntroducedVariables patternTypedNode =
    patternTypedNode.value
        |> patternIntroducedVariables


patternIntroducedVariables :
    ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type comparableTypeVariable_)
    -> FastSet.Set String
patternIntroducedVariables inferredPattern =
    case inferredPattern of
        ElmSyntaxTypeInfer.PatternIgnored ->
            FastSet.empty

        ElmSyntaxTypeInfer.PatternUnit ->
            FastSet.empty

        ElmSyntaxTypeInfer.PatternChar _ ->
            FastSet.empty

        ElmSyntaxTypeInfer.PatternString _ ->
            FastSet.empty

        ElmSyntaxTypeInfer.PatternInt _ ->
            FastSet.empty

        ElmSyntaxTypeInfer.PatternVariable variable ->
            FastSet.singleton variable

        ElmSyntaxTypeInfer.PatternParenthesized inParens ->
            patternTypedNodeIntroducedVariables
                inParens

        ElmSyntaxTypeInfer.PatternAs patternAs ->
            FastSet.insert patternAs.variable.value
                (patternAs.pattern
                    |> patternTypedNodeIntroducedVariables
                )

        ElmSyntaxTypeInfer.PatternTuple parts ->
            FastSet.union
                (parts.part0
                    |> patternTypedNodeIntroducedVariables
                )
                (parts.part1
                    |> patternTypedNodeIntroducedVariables
                )

        ElmSyntaxTypeInfer.PatternTriple parts ->
            FastSet.union
                (parts.part0
                    |> patternTypedNodeIntroducedVariables
                )
                (FastSet.union
                    (parts.part1
                        |> patternTypedNodeIntroducedVariables
                    )
                    (parts.part2
                        |> patternTypedNodeIntroducedVariables
                    )
                )

        ElmSyntaxTypeInfer.PatternListCons patternListCons ->
            FastSet.union
                (patternListCons.head
                    |> patternTypedNodeIntroducedVariables
                )
                (patternListCons.tail
                    |> patternTypedNodeIntroducedVariables
                )

        ElmSyntaxTypeInfer.PatternListExact elements ->
            elements
                |> listMapToFastSetsAndUnify
                    (\element ->
                        element
                            |> patternTypedNodeIntroducedVariables
                    )

        ElmSyntaxTypeInfer.PatternVariant patternVariant ->
            patternVariant.values
                |> listMapToFastSetsAndUnify
                    (\value ->
                        value
                            |> patternTypedNodeIntroducedVariables
                    )

        ElmSyntaxTypeInfer.PatternRecord fields ->
            fields
                |> List.foldl
                    (\fieldTypedNode soFar ->
                        soFar |> FastSet.insert fieldTypedNode.value
                    )
                    FastSet.empty


typeSetLocalToOrigin :
    Elm.Syntax.ModuleName.ModuleName
    -> ElmSyntaxTypeInfer.Type variable
    -> ElmSyntaxTypeInfer.Type variable
typeSetLocalToOrigin moduleOrigin inferredType =
    case inferredType of
        ElmSyntaxTypeInfer.TypeVariable variable ->
            ElmSyntaxTypeInfer.TypeVariable variable

        ElmSyntaxTypeInfer.TypeNotVariable typeNotVariable ->
            ElmSyntaxTypeInfer.TypeNotVariable
                (typeNotVariableSetLocalToOrigin moduleOrigin typeNotVariable)


typeNotVariableSetLocalToOrigin :
    Elm.Syntax.ModuleName.ModuleName
    -> ElmSyntaxTypeInfer.TypeNotVariable variable
    -> ElmSyntaxTypeInfer.TypeNotVariable variable
typeNotVariableSetLocalToOrigin moduleOrigin typeNotVariable =
    case typeNotVariable of
        ElmSyntaxTypeInfer.TypeConstruct typeConstruct ->
            ElmSyntaxTypeInfer.TypeConstruct
                { name = typeConstruct.name
                , moduleOrigin =
                    case typeConstruct.moduleOrigin of
                        [] ->
                            moduleOrigin

                        moduleOriginPart0 :: moduleOriginPart1Up ->
                            moduleOriginPart0 :: moduleOriginPart1Up
                , arguments =
                    typeConstruct.arguments
                        |> List.map
                            (\argument ->
                                argument
                                    |> typeSetLocalToOrigin moduleOrigin
                            )
                }

        ElmSyntaxTypeInfer.TypeUnit ->
            ElmSyntaxTypeInfer.TypeUnit

        ElmSyntaxTypeInfer.TypeFunction parts ->
            ElmSyntaxTypeInfer.TypeFunction
                { input =
                    parts.input
                        |> typeSetLocalToOrigin moduleOrigin
                , output =
                    parts.output
                        |> typeSetLocalToOrigin moduleOrigin
                }

        ElmSyntaxTypeInfer.TypeTuple parts ->
            ElmSyntaxTypeInfer.TypeTuple
                { part0 =
                    parts.part0
                        |> typeSetLocalToOrigin moduleOrigin
                , part1 =
                    parts.part1
                        |> typeSetLocalToOrigin moduleOrigin
                }

        ElmSyntaxTypeInfer.TypeTriple parts ->
            ElmSyntaxTypeInfer.TypeTriple
                { part0 =
                    parts.part0
                        |> typeSetLocalToOrigin moduleOrigin
                , part1 =
                    parts.part1
                        |> typeSetLocalToOrigin moduleOrigin
                , part2 =
                    parts.part2
                        |> typeSetLocalToOrigin moduleOrigin
                }

        ElmSyntaxTypeInfer.TypeRecord fields ->
            ElmSyntaxTypeInfer.TypeRecord
                (fields
                    |> FastDict.map
                        (\_ fieldValue ->
                            fieldValue
                                |> typeSetLocalToOrigin moduleOrigin
                        )
                )

        ElmSyntaxTypeInfer.TypeRecordExtension typeRecordExtension ->
            ElmSyntaxTypeInfer.TypeRecordExtension
                { recordVariable = typeRecordExtension.recordVariable
                , fields =
                    typeRecordExtension.fields
                        |> FastDict.map
                            (\_ fieldValue ->
                                fieldValue
                                    |> typeSetLocalToOrigin moduleOrigin
                            )
                }


fastDictMapToFastSetAndUnify :
    (value -> FastSet.Set comparableFastSetElement)
    -> FastDict.Dict key_ value
    -> FastSet.Set comparableFastSetElement
fastDictMapToFastSetAndUnify valueToFastSet fastDict =
    fastDict
        |> FastDict.foldl
            (\_ value soFar ->
                FastSet.union
                    (value |> valueToFastSet)
                    soFar
            )
            FastSet.empty


generatedFsharpRecordTypeAliasName : List String -> String
generatedFsharpRecordTypeAliasName recordFields =
    "Generated_" ++ (recordFields |> String.join "_")


moduleHeaderName : Elm.Syntax.Module.Module -> Elm.Syntax.ModuleName.ModuleName
moduleHeaderName moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName |> Elm.Syntax.Node.value


valueOrFunctionDeclaration :
    ModuleContext
    ->
        { declaration_
            | parameters :
                List
                    (ElmSyntaxTypeInfer.TypedNode
                        (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
                        (ElmSyntaxTypeInfer.Type String)
                    )
            , result :
                ElmSyntaxTypeInfer.TypedNode
                    (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type String))
                    (ElmSyntaxTypeInfer.Type String)
            , type_ : ElmSyntaxTypeInfer.Type String
        }
    ->
        Result
            String
            { result : FsharpExpression
            , type_ : FsharpType
            }
valueOrFunctionDeclaration moduleContext syntaxDeclarationValueOrFunction =
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    { type_ = maybeType
                    , result =
                        case parameters |> List.map (\param -> { type_ = param.type_, pattern = param.pattern }) of
                            [] ->
                                result

                            parameter0 :: parameter1Up ->
                                FsharpExpressionLambda
                                    { parameters = parameter0 :: parameter1Up
                                    , result = result
                                    }
                    }
                )
                (syntaxDeclarationValueOrFunction.result
                    |> expression
                        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            moduleContext.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                        , variantLookup = moduleContext.variantLookup
                        , recordTypeAliasLookup = moduleContext.recordTypeAliasLookup
                        , portIncomingLookup = moduleContext.portIncomingLookup
                        , portOutgoingLookup = moduleContext.portOutgoingLookup
                        , variablesFromWithinDeclarationInScope =
                            parameters
                                |> listMapToFastSetsAndUnify .introducedVariables
                        }
                )
        )
        (syntaxDeclarationValueOrFunction.parameters
            |> listMapAndCombineOk
                (\parameter -> parameter |> typedPattern)
        )
        (syntaxDeclarationValueOrFunction.type_
            |> type_
        )


variableNameDisambiguateFromFsharpKeywords : String -> String
variableNameDisambiguateFromFsharpKeywords variableName =
    if fsharpKeywords |> FastSet.member variableName then
        variableName ++ "_"

    else
        variableName


fsharpKeywords : FastSet.Set String
fsharpKeywords =
    -- https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/keyword-reference
    FastSet.fromList
        [ "abstract"
        , "and"
        , "as"
        , "assert"
        , "base"
        , "begin"
        , "class"
        , "default"
        , "delegate"
        , "do"
        , "done"
        , "downcast"
        , "downto"
        , "elif"
        , "else"
        , "end"
        , "extern"
        , "false"
        , "finally"
        , "fixed"
        , "for"
        , "fun"
        , "function"
        , "global"
        , "if"
        , "in"
        , "inherit"
        , "inline"
        , "interface"
        , "lazy"
        , "let"
        , "match"
        , "member"
        , "mod"
        , "sig"
        , "module"
        , "mutable"
        , "namespace"
        , "new"
        , "not"
        , "null"
        , "of"
        , "open"
        , "or"
        , "override"
        , "private"
        , "public"
        , "rec"
        , "return"
        , "select"
        , "static"
        , "struct"
        , "then"
        , "to"
        , "true"
        , "try"
        , "type"
        , "upcast"
        , "use"
        , "val"
        , "void"
        , "when"
        , "while"
        , "with"
        , "yield"
        , "const"
        , -- some exposed names
          "float"
        , "single"
        , "double"
        , "decimal"
        , "int"
        , "byte"
        , "char"
        , "string"
        , "unit"
        , "option"
        , "list"
        ]


expressionContextAddVariablesInScope :
    FastSet.Set String
    ->
        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , recordTypeAliasLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                (List String)
        , portIncomingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , portOutgoingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
    ->
        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , recordTypeAliasLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                (List String)
        , portIncomingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , portOutgoingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
expressionContextAddVariablesInScope additionalVariablesInScope context =
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
        context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
    , variantLookup = context.variantLookup
    , recordTypeAliasLookup = context.recordTypeAliasLookup
    , portIncomingLookup = context.portIncomingLookup
    , portOutgoingLookup = context.portOutgoingLookup
    , variablesFromWithinDeclarationInScope =
        FastSet.union
            additionalVariablesInScope
            context.variablesFromWithinDeclarationInScope
    }


expression :
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , recordTypeAliasLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            (List String)
    , portIncomingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , portOutgoingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    ->
        ElmSyntaxTypeInfer.TypedNode
            (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type String))
            (ElmSyntaxTypeInfer.Type String)
    -> Result String FsharpExpression
expression context expressionTypedNode =
    -- IGNORE TCO
    case expressionTypedNode.value of
        ElmSyntaxTypeInfer.ExpressionUnit ->
            Ok FsharpExpressionUnit

        ElmSyntaxTypeInfer.ExpressionInteger intValue ->
            case expressionTypedNode.type_ of
                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct typeConstruct) ->
                    case ( typeConstruct.moduleOrigin, typeConstruct.name ) of
                        ( [ "Basics" ], "Float" ) ->
                            Ok (FsharpExpressionFloat (intValue.value |> Basics.toFloat))

                        _ ->
                            -- assume Int
                            Ok (FsharpExpressionInt intValue.value)

                _ ->
                    -- assume Int
                    Ok (FsharpExpressionInt intValue.value)

        ElmSyntaxTypeInfer.ExpressionFloat floatValue ->
            Ok (FsharpExpressionFloat floatValue)

        ElmSyntaxTypeInfer.ExpressionChar charValue ->
            Ok (FsharpExpressionChar charValue)

        ElmSyntaxTypeInfer.ExpressionString stringValue ->
            Ok (FsharpExpressionString stringValue)

        ElmSyntaxTypeInfer.ExpressionRecordAccessFunction fieldName ->
            case expressionTypedNode.type_ of
                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                    Result.map
                        (\recordVariableFsharpType ->
                            let
                                recordVariableName : String
                                recordVariableName =
                                    "generated_record"
                            in
                            FsharpExpressionLambda
                                { parameters =
                                    [ { pattern = FsharpPatternVariable recordVariableName
                                      , type_ = recordVariableFsharpType
                                      }
                                    ]
                                , result =
                                    FsharpExpressionRecordAccess
                                        { record =
                                            FsharpExpressionReference
                                                { moduleOrigin = Nothing
                                                , name = recordVariableName
                                                }
                                        , field =
                                            fieldName
                                                |> String.replace "." ""
                                                |> stringFirstCharToUpper
                                        }
                                }
                        )
                        (typeFunction.input |> type_)

                _ ->
                    Err "record access function has an inferred type that wasn't a function"

        ElmSyntaxTypeInfer.ExpressionOperatorFunction operator ->
            Result.map
                (\operationFunctionReference ->
                    FsharpExpressionReference operationFunctionReference
                )
                (expressionOperatorToFsharpFunctionReference
                    { moduleOrigin = operator.moduleOrigin
                    , symbol = operator.symbol
                    , type_ = expressionTypedNode.type_
                    }
                )

        ElmSyntaxTypeInfer.ExpressionCall call ->
            Result.map3
                (\called argument0 argument1Up ->
                    condenseExpressionCall
                        { called = called
                        , argument0 = argument0
                        , argument1Up = argument1Up
                        }
                )
                (call.called |> expression context)
                (call.argument0 |> expression context)
                (call.argument1Up
                    |> listMapAndCombineOk
                        (\argument -> argument |> expression context)
                )

        ElmSyntaxTypeInfer.ExpressionInfixOperation infixOperation ->
            case infixOperation.operator.symbol of
                "|>" ->
                    Result.map2
                        (\argument called ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (infixOperation.left |> expression context)
                        (infixOperation.right |> expression context)

                "<|" ->
                    Result.map2
                        (\called argument ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (infixOperation.left |> expression context)
                        (infixOperation.right |> expression context)

                "++" ->
                    Result.map2
                        (\left right ->
                            if
                                infixOperation.left.type_
                                    == ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                            then
                                case left of
                                    FsharpExpressionString "" ->
                                        right

                                    leftNotStringEmpty ->
                                        case right of
                                            FsharpExpressionString "" ->
                                                leftNotStringEmpty

                                            rightNotStringEmpty ->
                                                FsharpExpressionCall
                                                    { called =
                                                        FsharpExpressionReference
                                                            { moduleOrigin = Nothing
                                                            , name = "String_append"
                                                            }
                                                    , arguments =
                                                        [ leftNotStringEmpty, rightNotStringEmpty ]
                                                    }

                            else
                                FsharpExpressionCall
                                    { called =
                                        FsharpExpressionReference
                                            { moduleOrigin = Just "List"
                                            , name = "append"
                                            }
                                    , arguments = [ left, right ]
                                    }
                        )
                        (infixOperation.left |> expression context)
                        (infixOperation.right |> expression context)

                _ ->
                    Result.map3
                        (\operationFunctionReference left right ->
                            FsharpExpressionCall
                                { called =
                                    FsharpExpressionReference operationFunctionReference
                                , arguments = [ left, right ]
                                }
                        )
                        (expressionOperatorToFsharpFunctionReference
                            infixOperation.operator
                        )
                        (infixOperation.left |> expression context)
                        (infixOperation.right |> expression context)

        ElmSyntaxTypeInfer.ExpressionReference reference ->
            let
                asVariableFromWithinDeclaration : Maybe String
                asVariableFromWithinDeclaration =
                    case reference.moduleOrigin of
                        _ :: _ ->
                            Nothing

                        [] ->
                            let
                                fsharpName : String
                                fsharpName =
                                    reference.name |> variableNameDisambiguateFromFsharpKeywords
                            in
                            if
                                context.variablesFromWithinDeclarationInScope
                                    |> FastSet.member fsharpName
                            then
                                Just fsharpName

                            else
                                Nothing
            in
            case asVariableFromWithinDeclaration of
                Just variableFromWithinDeclaration ->
                    Ok
                        (FsharpExpressionReference
                            { moduleOrigin = Nothing
                            , name = variableFromWithinDeclaration
                            }
                        )

                Nothing ->
                    case context.variantLookup |> FastDict.get ( reference.qualification, reference.name ) of
                        Just _ ->
                            let
                                fsharpReference : { moduleOrigin : Maybe String, name : String }
                                fsharpReference =
                                    case
                                        { moduleOrigin = reference.moduleOrigin
                                        , name = reference.name
                                        , type_ = expressionTypedNode.type_
                                        }
                                            |> referenceToCoreFsharp
                                    of
                                        Just fsharpCoreReference ->
                                            fsharpCoreReference

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                referenceToFsharpName
                                                    { moduleOrigin = reference.moduleOrigin
                                                    , name = reference.name
                                                    }
                                            }
                            in
                            case expressionTypedNode.type_ |> inferredTypeExpandFunction |> .inputs |> listMapAndCombineOk type_ of
                                Err error ->
                                    Err error

                                Ok [] ->
                                    Ok (FsharpExpressionReference fsharpReference)

                                Ok (valueType0 :: valueType1Up) ->
                                    let
                                        generatedValueVariableReference : Int -> FsharpExpression
                                        generatedValueVariableReference valueIndex =
                                            FsharpExpressionReference
                                                { moduleOrigin = Nothing
                                                , name =
                                                    "generated_"
                                                        ++ (valueIndex |> String.fromInt)
                                                }

                                        generatedValueTypedPattern : Int -> FsharpPattern
                                        generatedValueTypedPattern valueIndex =
                                            FsharpPatternVariable
                                                ("generated_"
                                                    ++ (valueIndex |> String.fromInt)
                                                )
                                    in
                                    Ok
                                        (FsharpExpressionLambda
                                            { parameters =
                                                { pattern = generatedValueTypedPattern 0
                                                , type_ = valueType0
                                                }
                                                    :: (valueType1Up
                                                            |> List.indexedMap
                                                                (\i valueType ->
                                                                    { pattern = generatedValueTypedPattern (i + 1)
                                                                    , type_ = valueType
                                                                    }
                                                                )
                                                       )
                                            , result =
                                                FsharpExpressionCall
                                                    { called = FsharpExpressionReference fsharpReference
                                                    , arguments =
                                                        [ case valueType1Up of
                                                            [] ->
                                                                generatedValueVariableReference 0

                                                            _ :: valueType2Up ->
                                                                FsharpExpressionTuple
                                                                    { part0 = generatedValueVariableReference 0
                                                                    , part1 = generatedValueVariableReference 1
                                                                    , part2Up =
                                                                        valueType2Up
                                                                            |> List.indexedMap
                                                                                (\i _ -> generatedValueVariableReference (i + 2))
                                                                    }
                                                        ]
                                                    }
                                            }
                                        )

                        Nothing ->
                            case context.recordTypeAliasLookup |> FastDict.get ( reference.qualification, reference.name ) of
                                Just fieldOrder ->
                                    case fieldOrder of
                                        [] ->
                                            Ok (FsharpExpressionRecord FastDict.empty)

                                        fieldName0 :: fieldName1Up ->
                                            Result.map
                                                (\fsharpType ->
                                                    let
                                                        parameterTypes : List FsharpType
                                                        parameterTypes =
                                                            fsharpTypeExpandFunctionIntoReverse []
                                                                fsharpType
                                                                |> List.drop 1
                                                                |> List.reverse
                                                    in
                                                    FsharpExpressionLambda
                                                        { parameters =
                                                            List.map2
                                                                (\fieldName fieldType ->
                                                                    { pattern = FsharpPatternVariable ("generated_" ++ fieldName)
                                                                    , type_ = fieldType
                                                                    }
                                                                )
                                                                (fieldName0 :: fieldName1Up)
                                                                parameterTypes
                                                        , result =
                                                            FsharpExpressionRecord
                                                                ((fieldName0 :: fieldName1Up)
                                                                    |> List.foldl
                                                                        (\fieldName soFar ->
                                                                            soFar
                                                                                |> FastDict.insert fieldName
                                                                                    (FsharpExpressionReference
                                                                                        { moduleOrigin = Nothing
                                                                                        , name = "generated_" ++ fieldName
                                                                                        }
                                                                                    )
                                                                        )
                                                                        FastDict.empty
                                                                )
                                                        }
                                                )
                                                (expressionTypedNode.type_ |> type_)

                                Nothing ->
                                    if context.portOutgoingLookup |> FastSet.member ( reference.qualification, reference.name ) then
                                        Ok
                                            (FsharpExpressionCall
                                                { called =
                                                    FsharpExpressionReference
                                                        { moduleOrigin = Nothing
                                                        , name = "PlatformCmd_portOutgoingWithName"
                                                        }
                                                , arguments =
                                                    [ FsharpExpressionString reference.name ]
                                                }
                                            )

                                    else if context.portIncomingLookup |> FastSet.member ( reference.qualification, reference.name ) then
                                        Ok
                                            (FsharpExpressionCall
                                                { called =
                                                    FsharpExpressionReference
                                                        { moduleOrigin = Nothing
                                                        , name = "PlatformSub_portIncomingWithName"
                                                        }
                                                , arguments =
                                                    [ FsharpExpressionString reference.name ]
                                                }
                                            )

                                    else
                                        Ok
                                            (FsharpExpressionReference
                                                (case
                                                    { moduleOrigin = reference.moduleOrigin
                                                    , name = reference.name
                                                    , type_ = expressionTypedNode.type_
                                                    }
                                                        |> referenceToCoreFsharp
                                                 of
                                                    Just fsharpReference ->
                                                        fsharpReference

                                                    Nothing ->
                                                        { moduleOrigin = Nothing
                                                        , name =
                                                            referenceToFsharpName
                                                                { moduleOrigin = reference.moduleOrigin
                                                                , name = reference.name
                                                                }
                                                        }
                                                )
                                            )

        ElmSyntaxTypeInfer.ExpressionIfThenElse ifThenElse ->
            Result.map3
                (\condition onTrue onFalse ->
                    FsharpExpressionIfElse
                        { condition = condition
                        , onTrue = onTrue
                        , onFalse = onFalse
                        }
                )
                (ifThenElse.condition |> expression context)
                (ifThenElse.onTrue |> expression context)
                (ifThenElse.onFalse |> expression context)

        ElmSyntaxTypeInfer.ExpressionParenthesized inParens ->
            inParens |> expression context

        ElmSyntaxTypeInfer.ExpressionNegation inNegationNode ->
            Result.map
                (\inNegation ->
                    FsharpExpressionCall
                        { called =
                            FsharpExpressionReference
                                (case inNegationNode.type_ of
                                    ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct inputTypeConstruct) ->
                                        case ( inputTypeConstruct.moduleOrigin, inputTypeConstruct.name ) of
                                            ( [ "Basics" ], "Float" ) ->
                                                { moduleOrigin = Nothing, name = "Basics_fnegate" }

                                            _ ->
                                                -- assume Int
                                                { moduleOrigin = Nothing, name = "Basics_inegate" }

                                    _ ->
                                        -- assume Int
                                        { moduleOrigin = Nothing, name = "Basics_inegate" }
                                )
                        , arguments = [ inNegation ]
                        }
                )
                (inNegationNode |> expression context)

        ElmSyntaxTypeInfer.ExpressionRecordAccess recordAccess ->
            Result.map
                (\record ->
                    FsharpExpressionRecordAccess
                        { record = record
                        , field =
                            recordAccess.fieldName
                                |> String.replace "." ""
                                |> stringFirstCharToUpper
                        }
                )
                (recordAccess.record |> expression context)

        ElmSyntaxTypeInfer.ExpressionTuple parts ->
            Result.map2
                (\part0 part1 ->
                    FsharpExpressionTuple
                        { part0 = part0
                        , part1 = part1
                        , part2Up = []
                        }
                )
                (parts.part0 |> expression context)
                (parts.part1 |> expression context)

        ElmSyntaxTypeInfer.ExpressionTriple parts ->
            Result.map3
                (\part0 part1 part2 ->
                    FsharpExpressionTuple
                        { part0 = part0
                        , part1 = part1
                        , part2Up = [ part2 ]
                        }
                )
                (parts.part0 |> expression context)
                (parts.part1 |> expression context)
                (parts.part2 |> expression context)

        ElmSyntaxTypeInfer.ExpressionList elementNodes ->
            Result.map (\elements -> FsharpExpressionList elements)
                (elementNodes
                    |> listMapAndCombineOk
                        (\element -> element |> expression context)
                )

        ElmSyntaxTypeInfer.ExpressionRecord fieldNodes ->
            Result.map (\fields -> FsharpExpressionRecord fields)
                (fieldNodes
                    |> listMapAndCombineOk
                        (\field ->
                            Result.map
                                (\fieldValue ->
                                    ( field.name
                                        |> stringFirstCharToUpper
                                    , fieldValue
                                    )
                                )
                                (field.value |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        ElmSyntaxTypeInfer.ExpressionRecordUpdate recordUpdate ->
            Result.map
                (\fields ->
                    let
                        fsharpRecordVariable : String
                        fsharpRecordVariable =
                            recordUpdate.recordVariable.value.name
                                |> variableNameDisambiguateFromFsharpKeywords
                    in
                    FsharpExpressionRecordUpdate
                        { originalRecordVariable =
                            referenceToFsharpName
                                { moduleOrigin =
                                    case
                                        context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                                            |> FastDict.get ( [], fsharpRecordVariable )
                                    of
                                        Nothing ->
                                            []

                                        Just moduleOrigin ->
                                            moduleOrigin
                                , name = fsharpRecordVariable
                                }
                        , fields = fields
                        }
                )
                ((recordUpdate.field0 :: recordUpdate.field1Up)
                    |> listMapAndCombineOk
                        (\field ->
                            Result.map
                                (\fieldValue ->
                                    ( field.name, fieldValue )
                                )
                                (field.value |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        ElmSyntaxTypeInfer.ExpressionLambda lambda ->
            resultAndThen2
                (\parameter0 parameter1Up ->
                    Result.map
                        (\result ->
                            FsharpExpressionLambda
                                { parameters =
                                    { pattern = parameter0.pattern
                                    , type_ = parameter0.type_
                                    }
                                        :: (parameter1Up
                                                |> List.map
                                                    (\fsharpParameter ->
                                                        { pattern = fsharpParameter.pattern
                                                        , type_ = fsharpParameter.type_
                                                        }
                                                    )
                                           )
                                , result = result
                                }
                        )
                        (lambda.result
                            |> expression
                                (context
                                    |> expressionContextAddVariablesInScope
                                        (FastSet.union
                                            parameter0.introducedVariables
                                            (parameter1Up
                                                |> listMapToFastSetsAndUnify .introducedVariables
                                            )
                                        )
                                )
                        )
                )
                (lambda.parameter0 |> typedPattern)
                (lambda.parameter1Up
                    |> listMapAndCombineOk
                        (\parameter -> parameter |> typedPattern)
                )

        ElmSyntaxTypeInfer.ExpressionCaseOf caseOf ->
            Result.map3
                (\matched case0 case1Up ->
                    FsharpExpressionMatchWith
                        { matched = matched
                        , case0 = case0
                        , case1Up = case1Up
                        }
                )
                (caseOf.matchedExpression |> expression context)
                (caseOf.case0 |> case_ context)
                (caseOf.case1Up
                    |> listMapAndCombineOk
                        (\parameter ->
                            parameter |> case_ context
                        )
                )

        ElmSyntaxTypeInfer.ExpressionLetIn letIn ->
            let
                variablesForWholeLetIn : FastSet.Set String
                variablesForWholeLetIn =
                    (letIn.declaration0 :: letIn.declaration1Up)
                        |> listMapToFastSetsAndUnify
                            (\syntaxLetDeclarationAndRange ->
                                case syntaxLetDeclarationAndRange.declaration of
                                    ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration syntaxLetValueOrFunction ->
                                        FastSet.singleton
                                            (syntaxLetValueOrFunction.name
                                                |> variableNameDisambiguateFromFsharpKeywords
                                            )

                                    ElmSyntaxTypeInfer.LetDestructuring syntaxLetDestructuring ->
                                        syntaxLetDestructuring.pattern
                                            |> patternBindings
                                            |> listMapAndToFastSet
                                                variableNameDisambiguateFromFsharpKeywords
                            )
            in
            Result.map3
                (\declaration0 declaration1Up result ->
                    FsharpExpressionWithLetDeclarations
                        { declaration0 = declaration0
                        , declaration1Up = declaration1Up
                        , result = result
                        }
                )
                (letIn.declaration0.declaration
                    |> letDeclaration
                        (context
                            |> expressionContextAddVariablesInScope
                                variablesForWholeLetIn
                        )
                )
                (letIn.declaration1Up
                    |> listMapAndCombineOk
                        (\letDecl ->
                            letDecl.declaration
                                |> letDeclaration
                                    (context
                                        |> expressionContextAddVariablesInScope
                                            variablesForWholeLetIn
                                    )
                        )
                )
                (letIn.result
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                variablesForWholeLetIn
                        )
                )


inferredTypeExpandFunction :
    ElmSyntaxTypeInfer.Type String
    ->
        { inputs : List (ElmSyntaxTypeInfer.Type String)
        , output : ElmSyntaxTypeInfer.Type String
        }
inferredTypeExpandFunction inferredType =
    case inferredType of
        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
            let
                outputExpanded :
                    { inputs : List (ElmSyntaxTypeInfer.Type String)
                    , output : ElmSyntaxTypeInfer.Type String
                    }
                outputExpanded =
                    typeFunction.output |> inferredTypeExpandFunction
            in
            { inputs = typeFunction.input :: outputExpanded.inputs
            , output = outputExpanded.output
            }

        typeNotFunction ->
            { inputs = [], output = typeNotFunction }


{-| Recursively find all introduced variables
in the [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)
(like `a` and `b` in `( Just a, { b } )`)
-}
patternBindings :
    ElmSyntaxTypeInfer.TypedNode
        (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
        (ElmSyntaxTypeInfer.Type String)
    -> List String
patternBindings syntaxPattern =
    -- IGNORE TCO
    case syntaxPattern.value of
        ElmSyntaxTypeInfer.PatternVariable name ->
            [ name ]

        ElmSyntaxTypeInfer.PatternAs patternAs ->
            patternAs.variable.value
                :: (patternAs.pattern |> patternBindings)

        ElmSyntaxTypeInfer.PatternParenthesized inParens ->
            inParens |> patternBindings

        ElmSyntaxTypeInfer.PatternListExact elements ->
            elements |> List.concatMap patternBindings

        ElmSyntaxTypeInfer.PatternTuple parts ->
            (parts.part0 |> patternBindings)
                ++ (parts.part1 |> patternBindings)

        ElmSyntaxTypeInfer.PatternTriple parts ->
            (parts.part0 |> patternBindings)
                ++ (parts.part1 |> patternBindings)
                ++ (parts.part2 |> patternBindings)

        ElmSyntaxTypeInfer.PatternRecord fields ->
            fields |> List.map .value

        ElmSyntaxTypeInfer.PatternVariant patternVariant ->
            patternVariant.values |> List.concatMap patternBindings

        ElmSyntaxTypeInfer.PatternListCons listCons ->
            (listCons.head |> patternBindings) ++ (listCons.head |> patternBindings)

        ElmSyntaxTypeInfer.PatternIgnored ->
            []

        ElmSyntaxTypeInfer.PatternUnit ->
            []

        ElmSyntaxTypeInfer.PatternChar _ ->
            []

        ElmSyntaxTypeInfer.PatternString _ ->
            []

        ElmSyntaxTypeInfer.PatternInt _ ->
            []


resultAndThen2 :
    (a -> b -> Result error c)
    -> Result error a
    -> Result error b
    -> Result error c
resultAndThen2 abToResult aResult bResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    abToResult a b


listMapToFastSetsAndUnify :
    (listElement -> FastSet.Set comparableFastSetElement)
    -> List listElement
    -> FastSet.Set comparableFastSetElement
listMapToFastSetsAndUnify elementToSet list =
    list
        |> List.foldl
            (\element soFar ->
                FastSet.union
                    (element |> elementToSet)
                    soFar
            )
            FastSet.empty


listMapAndToFastSet :
    (a -> comparable)
    -> List a
    -> FastSet.Set comparable
listMapAndToFastSet elementToSetElement list =
    list
        |> List.foldl
            (\element soFar ->
                soFar
                    |> FastSet.insert
                        (element |> elementToSetElement)
            )
            FastSet.empty


condenseExpressionCall :
    { called : FsharpExpression
    , argument0 : FsharpExpression
    , argument1Up : List FsharpExpression
    }
    -> FsharpExpression
condenseExpressionCall call =
    case call.called of
        FsharpExpressionCall calledCall ->
            case calledCall.arguments of
                [] ->
                    FsharpExpressionCall
                        { called = call.called
                        , arguments = call.argument0 :: call.argument1Up
                        }

                calledCallArgument0 :: calledCallArgument1Up ->
                    condenseExpressionCall
                        { called = calledCall.called
                        , argument0 = calledCallArgument0
                        , argument1Up =
                            calledCallArgument1Up
                                ++ (call.argument0 :: call.argument1Up)
                        }

        FsharpExpressionLambda calledLambda ->
            case ( calledLambda.parameters |> List.map .pattern, calledLambda.result ) of
                ( (FsharpPatternVariable "generated_record") :: _, FsharpExpressionRecordAccess recordAccess ) ->
                    case call.argument1Up of
                        [] ->
                            FsharpExpressionRecordAccess
                                { record = call.argument0
                                , field = recordAccess.field
                                }

                        argument1 :: argument2Up ->
                            FsharpExpressionCall
                                { called =
                                    FsharpExpressionRecordAccess
                                        { record = call.argument0
                                        , field = recordAccess.field
                                        }
                                , arguments = argument1 :: argument2Up
                                }

                ( (FsharpPatternVariable "generated_0") :: lambdaParameter1Up, FsharpExpressionCall variantCall ) ->
                    if (lambdaParameter1Up |> List.length) == (call.argument1Up |> List.length) then
                        FsharpExpressionCall
                            { called = variantCall.called
                            , arguments =
                                [ case call.argument1Up of
                                    [] ->
                                        call.argument0

                                    callArgument1 :: callArgument2Up ->
                                        FsharpExpressionTuple
                                            { part0 = call.argument0
                                            , part1 = callArgument1
                                            , part2Up = callArgument2Up
                                            }
                                ]
                            }

                    else
                        FsharpExpressionCall
                            { called = FsharpExpressionLambda calledLambda
                            , arguments = call.argument0 :: call.argument1Up
                            }

                _ ->
                    FsharpExpressionCall
                        { called = FsharpExpressionLambda calledLambda
                        , arguments = call.argument0 :: call.argument1Up
                        }

        calledNotCall ->
            FsharpExpressionCall
                { called = calledNotCall
                , arguments = call.argument0 :: call.argument1Up
                }


case_ :
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , recordTypeAliasLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            (List String)
    , portIncomingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , portOutgoingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    ->
        { pattern :
            ElmSyntaxTypeInfer.TypedNode
                (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
                (ElmSyntaxTypeInfer.Type String)
        , result :
            ElmSyntaxTypeInfer.TypedNode
                (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type String))
                (ElmSyntaxTypeInfer.Type String)
        }
    ->
        Result
            String
            { pattern : FsharpPattern
            , patternType : FsharpType
            , result : FsharpExpression
            }
case_ context syntaxCase =
    Result.andThen
        (\casePattern ->
            Result.map
                (\result ->
                    { pattern = casePattern.pattern
                    , patternType = casePattern.type_
                    , result = result
                    }
                )
                (syntaxCase.result
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                casePattern.introducedVariables
                        )
                )
        )
        (syntaxCase.pattern |> typedPattern)


letDeclaration :
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , recordTypeAliasLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            (List String)
    , portIncomingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , portOutgoingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> ElmSyntaxTypeInfer.LetDeclaration (ElmSyntaxTypeInfer.Type String)
    -> Result String FsharpLetDeclaration
letDeclaration context syntaxLetDeclaration =
    case syntaxLetDeclaration of
        ElmSyntaxTypeInfer.LetDestructuring letDestructuring ->
            Result.map2
                (\destructuringPattern destructuringExpression ->
                    FsharpLetDestructuring
                        { pattern = destructuringPattern.pattern
                        , patternType = destructuringPattern.type_
                        , expression = destructuringExpression
                        }
                )
                (letDestructuring.pattern |> typedPattern)
                (letDestructuring.expression |> expression context)

        ElmSyntaxTypeInfer.LetValueOrFunctionDeclaration letValueOrFunction ->
            Result.map
                FsharpLetDeclarationValueOrFunction
                (letValueOrFunction
                    |> letValueOrFunctionDeclaration context
                )


letValueOrFunctionDeclaration :
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , recordTypeAliasLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            (List String)
    , portIncomingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , portOutgoingLookup : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    ->
        { signature :
            Maybe
                { range : Elm.Syntax.Range.Range
                , nameRange : Elm.Syntax.Range.Range
                , annotationType : Elm.Syntax.TypeAnnotation.TypeAnnotation
                , annotationTypeRange : Elm.Syntax.Range.Range
                }
        , nameRange : Elm.Syntax.Range.Range
        , name : String
        , parameters : List (ElmSyntaxTypeInfer.TypedNode (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String)) (ElmSyntaxTypeInfer.Type String))
        , result : ElmSyntaxTypeInfer.TypedNode (ElmSyntaxTypeInfer.Expression (ElmSyntaxTypeInfer.Type String)) (ElmSyntaxTypeInfer.Type String)
        , type_ : ElmSyntaxTypeInfer.Type String
        }
    ->
        Result
            String
            { name : String
            , result : FsharpExpression
            , type_ : FsharpType
            }
letValueOrFunctionDeclaration context syntaxLetDeclarationValueOrFunction =
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    { name =
                        syntaxLetDeclarationValueOrFunction.name
                            |> variableNameDisambiguateFromFsharpKeywords
                    , type_ = maybeType
                    , result =
                        case parameters of
                            [] ->
                                result

                            parameter0 :: parameter1Up ->
                                FsharpExpressionLambda
                                    { parameters =
                                        { pattern = parameter0.pattern
                                        , type_ = parameter0.type_
                                        }
                                            :: (parameter1Up
                                                    |> List.map
                                                        (\parameter ->
                                                            { pattern = parameter.pattern
                                                            , type_ = parameter.type_
                                                            }
                                                        )
                                               )
                                    , result = result
                                    }
                    }
                )
                (syntaxLetDeclarationValueOrFunction.result
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                (parameters
                                    |> listMapToFastSetsAndUnify .introducedVariables
                                )
                        )
                )
        )
        (syntaxLetDeclarationValueOrFunction.parameters
            |> listMapAndCombineOk
                (\p -> p |> typedPattern)
        )
        (syntaxLetDeclarationValueOrFunction.type_ |> type_)


expressionOperatorToFsharpFunctionReference :
    { symbol : String
    , moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , type_ : ElmSyntaxTypeInfer.Type String
    }
    -> Result String { moduleOrigin : Maybe String, name : String }
expressionOperatorToFsharpFunctionReference operator =
    case operator.symbol of
        "+" ->
            Ok { moduleOrigin = Nothing, name = "(+)" }

        "-" ->
            Ok { moduleOrigin = Nothing, name = "(-)" }

        "*" ->
            Ok { moduleOrigin = Nothing, name = "(*)" }

        "/" ->
            Ok { moduleOrigin = Nothing, name = "(/)" }

        "//" ->
            Ok { moduleOrigin = Nothing, name = "(/)" }

        "^" ->
            case operator.type_ of
                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                    case typeFunction.input of
                        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct inputTypeConstruct) ->
                            case ( inputTypeConstruct.moduleOrigin, inputTypeConstruct.name ) of
                                ( [ "Basics" ], "Float" ) ->
                                    Ok { moduleOrigin = Nothing, name = "Basics_fpow" }

                                _ ->
                                    -- assume Int
                                    Ok { moduleOrigin = Nothing, name = "Basics_ipow" }

                        _ ->
                            -- assume Int
                            Ok { moduleOrigin = Nothing, name = "Basics_ipow" }

                _ ->
                    -- assume Int
                    Ok { moduleOrigin = Nothing, name = "Basics_ipow" }

        "==" ->
            Ok { moduleOrigin = Nothing, name = "Basics_eq" }

        "/=" ->
            Ok { moduleOrigin = Nothing, name = "Basics_neq" }

        "||" ->
            Ok { moduleOrigin = Nothing, name = "Basics_or" }

        "&&" ->
            Ok { moduleOrigin = Nothing, name = "Basics_and" }

        "<" ->
            Ok { moduleOrigin = Nothing, name = "(<)" }

        ">" ->
            Ok { moduleOrigin = Nothing, name = "(>)" }

        "<=" ->
            Ok { moduleOrigin = Nothing, name = "(<=)" }

        ">=" ->
            Ok { moduleOrigin = Nothing, name = "(>=)" }

        "::" ->
            Ok { moduleOrigin = Nothing, name = "List_cons" }

        "++" ->
            case operator.type_ of
                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                    if
                        typeFunction.input
                            == ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "String" ]
                                    , name = "String"
                                    , arguments = []
                                    }
                                )
                    then
                        Ok { moduleOrigin = Nothing, name = "String_append" }

                    else
                        -- assume List
                        Ok { moduleOrigin = Just "List", name = "append" }

                _ ->
                    -- assume List
                    Ok { moduleOrigin = Just "List", name = "append" }

        "|>" ->
            Ok { moduleOrigin = Nothing, name = "(|>)" }

        "<|" ->
            Ok { moduleOrigin = Nothing, name = "(<|)" }

        ">>" ->
            Ok { moduleOrigin = Nothing, name = "(>>)" }

        "<<" ->
            Ok { moduleOrigin = Nothing, name = "(<<)" }

        "|=" ->
            Ok { moduleOrigin = Nothing, name = "ParserAdvanced_keeper" }

        "|." ->
            Ok { moduleOrigin = Nothing, name = "ParserAdvanced_ignorer" }

        "</>" ->
            Ok { moduleOrigin = Nothing, name = "UrlParser_slash" }

        "<?>" ->
            Ok { moduleOrigin = Nothing, name = "UrlParser_questionMark" }

        unknownOrUnsupportedOperator ->
            Err ("unknown/unsupported operator " ++ unknownOrUnsupportedOperator)


{-| Print a fsharp value/function declaration
-}
printFsharpValueOrFunctionDeclaration :
    { name : String
    , result : FsharpExpression
    , type_ : FsharpType
    }
    -> Print
printFsharpValueOrFunctionDeclaration fsharpValueOrFunctionDeclaration =
    case fsharpValueOrFunctionDeclaration.result of
        FsharpExpressionLambda function ->
            let
                parameterPrints : List Print
                parameterPrints =
                    function.parameters
                        |> List.map
                            (\parameter ->
                                let
                                    parameterTypePrint : Print
                                    parameterTypePrint =
                                        printFsharpTypeNotParenthesized
                                            parameter.type_
                                in
                                printParenthesized
                                    { opening = "("
                                    , closing = ")"
                                    , inner =
                                        (parameter.pattern
                                            |> printFsharpPatternParenthesizedIfSpaceSeparated
                                        )
                                            |> Print.followedBy (Print.exactly ":")
                                            |> Print.followedBy
                                                (Print.withIndentIncreasedBy 1
                                                    (Print.withIndentAtNextMultipleOf4
                                                        (Print.spaceOrLinebreakIndented
                                                            (parameterTypePrint |> Print.lineSpread)
                                                            |> Print.followedBy
                                                                parameterTypePrint
                                                        )
                                                    )
                                                )
                                    }
                            )

                parametersLineSpread : Print.LineSpread
                parametersLineSpread =
                    parameterPrints
                        |> Print.lineSpreadListMapAndCombine
                            Print.lineSpread
            in
            Print.exactly
                fsharpValueOrFunctionDeclaration.name
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 4
                        (parameterPrints
                            |> Print.listMapAndIntersperseAndFlatten
                                (\parameterPrint ->
                                    Print.spaceOrLinebreakIndented parametersLineSpread
                                        |> Print.followedBy parameterPrint
                                )
                                Print.empty
                        )
                    )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.exactly " ="
                            |> Print.followedBy
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (printFsharpExpressionNotParenthesized
                                            function.result
                                        )
                                )
                        )
                    )

        resultNotFunction ->
            Print.exactly
                fsharpValueOrFunctionDeclaration.name
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        ((let
                            typePrint : Print
                            typePrint =
                                printFsharpTypeNotParenthesized fsharpValueOrFunctionDeclaration.type_

                            fullLineSpread : Print.LineSpread
                            fullLineSpread =
                                typePrint |> Print.lineSpread
                          in
                          Print.exactly ":"
                            |> Print.followedBy
                                (Print.withIndentAtNextMultipleOf4
                                    (Print.spaceOrLinebreakIndented fullLineSpread
                                        |> Print.followedBy typePrint
                                    )
                                )
                         )
                            |> Print.followedBy
                                (Print.exactly " =")
                            |> Print.followedBy
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (printFsharpExpressionParenthesizedIfWithLetDeclarations
                                            resultNotFunction
                                        )
                                )
                        )
                    )


printFsharpLocalLetValueOrFunctionDeclaration :
    { name : String
    , result : FsharpExpression
    , type_ : FsharpType
    }
    -> Print
printFsharpLocalLetValueOrFunctionDeclaration fsharpValueOrFunctionDeclaration =
    case fsharpValueOrFunctionDeclaration.result of
        FsharpExpressionLambda function ->
            let
                parameterPrints : List Print
                parameterPrints =
                    function.parameters
                        |> List.map
                            (\parameter ->
                                let
                                    parameterTypePrint : Print
                                    parameterTypePrint =
                                        printFsharpTypeNotParenthesized
                                            parameter.type_
                                in
                                printParenthesized
                                    { opening = "("
                                    , closing = ")"
                                    , inner =
                                        parameter.pattern
                                            |> printFsharpPatternParenthesizedIfSpaceSeparated
                                            |> Print.followedBy (Print.exactly ":")
                                            |> Print.followedBy
                                                (Print.withIndentIncreasedBy 1
                                                    (Print.withIndentAtNextMultipleOf4
                                                        (Print.spaceOrLinebreakIndented
                                                            (parameterTypePrint |> Print.lineSpread)
                                                            |> Print.followedBy
                                                                parameterTypePrint
                                                        )
                                                    )
                                                )
                                    }
                            )

                parametersLineSpread : Print.LineSpread
                parametersLineSpread =
                    parameterPrints
                        |> Print.lineSpreadListMapAndCombine
                            Print.lineSpread
            in
            Print.exactly
                fsharpValueOrFunctionDeclaration.name
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 4
                        (Print.spaceOrLinebreakIndented parametersLineSpread
                            |> Print.followedBy
                                (parameterPrints
                                    |> Print.listMapAndIntersperseAndFlatten
                                        (\parameterPrint -> parameterPrint)
                                        (Print.spaceOrLinebreakIndented parametersLineSpread)
                                )
                        )
                    )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.exactly " ="
                            |> Print.followedBy
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (printFsharpExpressionNotParenthesized
                                            function.result
                                        )
                                )
                        )
                    )

        resultNotFunction ->
            Print.exactly
                fsharpValueOrFunctionDeclaration.name
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        ((let
                            typePrint : Print
                            typePrint =
                                printFsharpTypeNotParenthesized fsharpValueOrFunctionDeclaration.type_

                            fullLineSpread : Print.LineSpread
                            fullLineSpread =
                                typePrint |> Print.lineSpread
                          in
                          Print.exactly ":"
                            |> Print.followedBy
                                (Print.withIndentAtNextMultipleOf4
                                    (Print.spaceOrLinebreakIndented fullLineSpread
                                        |> Print.followedBy typePrint
                                    )
                                )
                         )
                            |> Print.followedBy
                                (Print.exactly " =")
                            |> Print.followedBy
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (printFsharpExpressionParenthesizedIfWithLetDeclarations
                                            resultNotFunction
                                        )
                                )
                        )
                    )


type FsharpValueOrFunctionDependencyBucket element
    = FsharpValueOrFunctionDependencySingle element
    | FsharpValueOrFunctionDependencyRecursiveBucket (List element)


fsharpValueOrFunctionDeclarationsGroupByDependencies :
    List
        { name : String
        , result : FsharpExpression
        , type_ : FsharpType
        }
    ->
        { mostToLeastDependedOn :
            List
                (FsharpValueOrFunctionDependencyBucket
                    { name : String
                    , result : FsharpExpression
                    , type_ : FsharpType
                    }
                )
        }
fsharpValueOrFunctionDeclarationsGroupByDependencies fsharpValueOrFunctionDeclarations =
    let
        ordered :
            List
                (Data.Graph.SCC
                    { name : String
                    , result : FsharpExpression
                    , type_ : FsharpType
                    }
                )
        ordered =
            Data.Graph.stronglyConnComp
                (fsharpValueOrFunctionDeclarations
                    |> List.map
                        (\fsharpValueOrFunctionDeclaration ->
                            ( fsharpValueOrFunctionDeclaration
                            , fsharpValueOrFunctionDeclaration.name
                            , fsharpValueOrFunctionDeclaration.result
                                |> fsharpExpressionContainedLocalReferences
                                |> FastSet.toList
                            )
                        )
                )
    in
    { mostToLeastDependedOn =
        ordered
            |> List.map
                (\fsharpValueOrFunctionDependencyGroup ->
                    case fsharpValueOrFunctionDependencyGroup of
                        Data.Graph.CyclicSCC recursiveGroup ->
                            FsharpValueOrFunctionDependencyRecursiveBucket recursiveGroup

                        Data.Graph.AcyclicSCC single ->
                            FsharpValueOrFunctionDependencySingle single
                )
    }


type FsharpChoiceTypeOrTypeAliasDeclaration
    = FsharpChoiceTypeDeclaration
        { name : String
        , parameters : List String
        , variants : FastDict.Dict String (Maybe FsharpType)
        }
    | FsharpTypeAliasDeclaration
        { name : String
        , parameters : List String
        , type_ : FsharpType
        }


fsharpTypeDeclarationsGroupByDependencies :
    { typeAliases :
        List
            { name : String
            , parameters : List String
            , type_ : FsharpType
            }
    , enums :
        List
            { name : String
            , parameters : List String
            , variants : FastDict.Dict String (Maybe FsharpType)
            }
    }
    ->
        { mostToLeastDependedOn :
            List
                (FsharpValueOrFunctionDependencyBucket
                    FsharpChoiceTypeOrTypeAliasDeclaration
                )
        }
fsharpTypeDeclarationsGroupByDependencies fsharpTypeDeclarations =
    let
        ordered : List (Data.Graph.SCC FsharpChoiceTypeOrTypeAliasDeclaration)
        ordered =
            Data.Graph.stronglyConnComp
                ((fsharpTypeDeclarations.typeAliases
                    |> List.map
                        (\aliasDeclaration ->
                            ( FsharpTypeAliasDeclaration aliasDeclaration
                            , aliasDeclaration.name
                            , aliasDeclaration.type_
                                |> fsharpTypeContainedLocalReferences
                                |> FastSet.toList
                            )
                        )
                 )
                    ++ (fsharpTypeDeclarations.enums
                            |> List.map
                                (\enumDeclaration ->
                                    ( FsharpChoiceTypeDeclaration enumDeclaration
                                    , enumDeclaration.name
                                    , enumDeclaration.variants
                                        |> FastDict.foldl
                                            (\_ maybeVariantValue soFar ->
                                                case maybeVariantValue of
                                                    Nothing ->
                                                        soFar

                                                    Just variantValue ->
                                                        FastSet.union
                                                            (variantValue
                                                                |> fsharpTypeContainedLocalReferences
                                                            )
                                                            soFar
                                            )
                                            FastSet.empty
                                        |> FastSet.toList
                                    )
                                )
                       )
                )
    in
    { mostToLeastDependedOn =
        ordered
            |> List.map
                (\fsharpValueOrFunctionDependencyGroup ->
                    case fsharpValueOrFunctionDependencyGroup of
                        Data.Graph.CyclicSCC recursiveGroup ->
                            FsharpValueOrFunctionDependencyRecursiveBucket recursiveGroup

                        Data.Graph.AcyclicSCC single ->
                            FsharpValueOrFunctionDependencySingle single
                )
    }


fsharpTypeContainedVariables : FsharpType -> FastSet.Set String
fsharpTypeContainedVariables fsharpType =
    -- IGNORE TCO
    case fsharpType of
        FsharpTypeVariable variable ->
            FastSet.singleton variable

        FsharpTypeTuple parts ->
            FastSet.union
                (parts.part0 |> fsharpTypeContainedVariables)
                (FastSet.union
                    (parts.part1 |> fsharpTypeContainedVariables)
                    (parts.part2Up
                        |> listMapToFastSetsAndUnify
                            fsharpTypeContainedVariables
                    )
                )

        FsharpTypeConstruct typeConstruct ->
            typeConstruct.arguments
                |> listMapToFastSetsAndUnify fsharpTypeContainedVariables

        FsharpTypeFunction typeFunction ->
            FastSet.union
                (typeFunction.input |> fsharpTypeContainedVariables)
                (typeFunction.output |> fsharpTypeContainedVariables)


fsharpTypeContainedLocalReferences : FsharpType -> FastSet.Set String
fsharpTypeContainedLocalReferences fsharpType =
    -- IGNORE TCO
    case fsharpType of
        FsharpTypeVariable _ ->
            FastSet.empty

        FsharpTypeTuple parts ->
            FastSet.union
                (parts.part0 |> fsharpTypeContainedLocalReferences)
                (FastSet.union
                    (parts.part1 |> fsharpTypeContainedLocalReferences)
                    (parts.part2Up
                        |> listMapToFastSetsAndUnify
                            fsharpTypeContainedLocalReferences
                    )
                )

        FsharpTypeConstruct typeConstruct ->
            FastSet.union
                (case typeConstruct.moduleOrigin of
                    Nothing ->
                        FastSet.singleton typeConstruct.name

                    Just _ ->
                        FastSet.empty
                )
                (typeConstruct.arguments
                    |> listMapToFastSetsAndUnify fsharpTypeContainedLocalReferences
                )

        FsharpTypeFunction typeFunction ->
            FastSet.union
                (typeFunction.input |> fsharpTypeContainedLocalReferences)
                (typeFunction.output |> fsharpTypeContainedLocalReferences)


qualifiedReferenceToFsharpName :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
qualifiedReferenceToFsharpName reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleOrigin ->
            moduleOrigin
                ++ "."
                ++ reference.name


printFsharpExpressionParenthesizedIfSpaceSeparated : FsharpExpression -> Print
printFsharpExpressionParenthesizedIfSpaceSeparated fsharpExpression =
    if fsharpExpression |> fsharpExpressionIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = printFsharpExpressionNotParenthesized fsharpExpression
            }

    else
        printFsharpExpressionNotParenthesized fsharpExpression


printFsharpExpressionParenthesizedIfWithLetDeclarations : FsharpExpression -> Print
printFsharpExpressionParenthesizedIfWithLetDeclarations fsharpExpression =
    case fsharpExpression of
        FsharpExpressionWithLetDeclarations fsharpExpressionWithLetDeclarations ->
            printParenthesized
                { opening = "("
                , closing = ")"
                , inner =
                    printFsharpExpressionWithLetDeclarations
                        fsharpExpressionWithLetDeclarations
                }

        fsharpExpressionNotWithLetDeclarations ->
            printFsharpExpressionNotParenthesized fsharpExpressionNotWithLetDeclarations


fsharpExpressionIsSpaceSeparated : FsharpExpression -> Bool
fsharpExpressionIsSpaceSeparated fsharpExpression =
    case fsharpExpression of
        FsharpExpressionUnit ->
            False

        FsharpExpressionChar _ ->
            False

        FsharpExpressionInt _ ->
            False

        FsharpExpressionFloat _ ->
            False

        FsharpExpressionString _ ->
            False

        FsharpExpressionReference _ ->
            False

        FsharpExpressionRecordAccess _ ->
            False

        FsharpExpressionTuple _ ->
            False

        FsharpExpressionIfElse _ ->
            True

        FsharpExpressionList _ ->
            False

        FsharpExpressionRecord _ ->
            False

        FsharpExpressionRecordUpdate _ ->
            False

        FsharpExpressionCall _ ->
            True

        FsharpExpressionLambda _ ->
            True

        FsharpExpressionMatchWith _ ->
            True

        FsharpExpressionWithLetDeclarations _ ->
            True


{-| Print a [`FsharpExpression`](#FsharpExpression)
-}
printFsharpExpressionNotParenthesized : FsharpExpression -> Print
printFsharpExpressionNotParenthesized fsharpExpression =
    -- IGNORE TCO
    case fsharpExpression of
        FsharpExpressionUnit ->
            Print.exactly "()"

        FsharpExpressionCall call ->
            printFsharpExpressionCall call

        FsharpExpressionReference reference ->
            Print.exactly
                (reference |> qualifiedReferenceToFsharpName)

        FsharpExpressionIfElse ifElse ->
            printFsharpExpressionIfElse ifElse

        FsharpExpressionChar charValue ->
            Print.exactly (charLiteral charValue)

        FsharpExpressionInt int ->
            Print.exactly (int |> intLiteral)

        FsharpExpressionFloat float ->
            Print.exactly (float |> floatLiteral)

        FsharpExpressionString string ->
            printFsharpString string

        FsharpExpressionTuple parts ->
            printFsharpExpressionTuple parts

        FsharpExpressionWithLetDeclarations expressionWithLetDeclarations ->
            printFsharpExpressionWithLetDeclarations expressionWithLetDeclarations

        FsharpExpressionMatchWith syntaxMatch ->
            printFsharpExpressionMatchWith syntaxMatch

        FsharpExpressionLambda syntaxLambda ->
            printFsharpExpressionLambda syntaxLambda

        FsharpExpressionRecord fields ->
            printFsharpExpressionRecord fields

        FsharpExpressionList elements ->
            printFsharpExpressionList elements

        FsharpExpressionRecordAccess syntaxRecordAccess ->
            printFsharpExpressionParenthesizedIfSpaceSeparated
                syntaxRecordAccess.record
                |> Print.followedBy
                    (Print.exactly
                        ("." ++ syntaxRecordAccess.field)
                    )

        FsharpExpressionRecordUpdate syntaxRecordUpdate ->
            printFsharpExpressionRecordUpdate syntaxRecordUpdate


printFsharpExpressionTuple :
    { part0 : FsharpExpression
    , part1 : FsharpExpression
    , part2Up : List FsharpExpression
    }
    -> Print
printFsharpExpressionTuple parts =
    let
        part0Print : Print
        part0Print =
            printFsharpExpressionNotParenthesized
                parts.part0

        part1Print : Print
        part1Print =
            printFsharpExpressionNotParenthesized
                parts.part1

        part2UpPrints : List Print
        part2UpPrints =
            parts.part2Up
                |> List.map printFsharpExpressionNotParenthesized

        lineSpread : Print.LineSpread
        lineSpread =
            part0Print
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> part1Print |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        part2UpPrints
                            |> Print.lineSpreadListMapAndCombine
                                Print.lineSpread
                    )
    in
    Print.exactly "(struct("
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (Print.spaceOrLinebreakIndented lineSpread)
            )
        |> Print.followedBy
            ((part0Print :: part1Print :: part2UpPrints)
                |> Print.listMapAndIntersperseAndFlatten
                    (\partPrint ->
                        Print.withIndentIncreasedBy 2 partPrint
                    )
                    (Print.emptyOrLinebreakIndented lineSpread
                        |> Print.followedBy
                            (Print.exactly ", ")
                    )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy (Print.exactly "))")


printFsharpExpressionCall :
    { called : FsharpExpression
    , arguments : List FsharpExpression
    }
    -> Print
printFsharpExpressionCall call =
    let
        calledPrint : Print
        calledPrint =
            printFsharpExpressionParenthesizedIfSpaceSeparated
                call.called

        argumentPrints : List Print
        argumentPrints =
            call.arguments
                |> List.map
                    printFsharpExpressionParenthesizedIfSpaceSeparated

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            argumentPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> calledPrint |> Print.lineSpread)
    in
    calledPrint
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (argumentPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.spaceOrLinebreakIndented fullLineSpread)
                        )
                )
            )


printFsharpExpressionList : List FsharpExpression -> Print
printFsharpExpressionList listElements =
    case listElements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            let
                elementsPrint : Print
                elementsPrint =
                    (element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\element ->
                                printFsharpExpressionNotParenthesized element
                            )
                            (Print.exactly ";"
                                |> Print.followedBy Print.linebreakIndented
                            )
            in
            Print.exactly "[ "
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 2
                        elementsPrint
                    )
                |> Print.followedBy
                    (Print.spaceOrLinebreakIndented
                        (elementsPrint |> Print.lineSpread)
                    )
                |> Print.followedBy
                    (Print.exactly "]")


printFsharpExpressionRecordUpdate :
    { originalRecordVariable : String
    , fields : FastDict.Dict String FsharpExpression
    }
    -> Print
printFsharpExpressionRecordUpdate syntaxRecordUpdate =
    Print.exactly "{ "
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (Print.exactly syntaxRecordUpdate.originalRecordVariable)
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy (Print.exactly " with ")
                    |> Print.followedBy
                        (syntaxRecordUpdate.fields
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( fieldName, fieldValue ) ->
                                    Print.withIndentIncreasedBy 2
                                        (Print.exactly (fieldName ++ " ="))
                                        |> Print.followedBy
                                            (Print.withIndentAtNextMultipleOf4
                                                (Print.linebreakIndented
                                                    |> Print.followedBy
                                                        (printFsharpExpressionNotParenthesized fieldValue)
                                                )
                                            )
                                )
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (Print.exactly "; ")
                                )
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


patternIsSpaceSeparated : FsharpPattern -> Bool
patternIsSpaceSeparated fsharpPattern =
    case fsharpPattern of
        FsharpPatternIgnore ->
            False

        FsharpPatternInt _ ->
            False

        FsharpPatternChar _ ->
            False

        FsharpPatternString _ ->
            False

        FsharpPatternVariable _ ->
            False

        FsharpPatternAs _ ->
            True

        FsharpPatternListCons _ ->
            True

        FsharpPatternListExact _ ->
            False

        FsharpPatternRecordInexhaustive _ ->
            False

        FsharpPatternVariant _ ->
            False

        FsharpPatternTuple _ ->
            False


printFsharpPatternParenthesizedIfSpaceSeparated : FsharpPattern -> Print
printFsharpPatternParenthesizedIfSpaceSeparated fsharpPattern =
    if fsharpPattern |> patternIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = fsharpPattern |> printFsharpPatternNotParenthesized
            }

    else
        fsharpPattern |> printFsharpPatternNotParenthesized


printFsharpExpressionLambda :
    { parameters : List { pattern : FsharpPattern, type_ : FsharpType }
    , result : FsharpExpression
    }
    -> Print
printFsharpExpressionLambda syntaxLambda =
    let
        parameterPrints : List Print
        parameterPrints =
            syntaxLambda.parameters
                |> List.map
                    (\parameter ->
                        let
                            parameterTypePrint : Print
                            parameterTypePrint =
                                printFsharpTypeNotParenthesized
                                    parameter.type_
                        in
                        printParenthesized
                            { opening = "("
                            , closing = ")"
                            , inner =
                                (parameter.pattern
                                    |> printFsharpPatternParenthesizedIfSpaceSeparated
                                )
                                    |> Print.followedBy (Print.exactly ":")
                                    |> Print.followedBy
                                        (Print.withIndentIncreasedBy 1
                                            (Print.withIndentAtNextMultipleOf4
                                                (Print.spaceOrLinebreakIndented
                                                    (parameterTypePrint |> Print.lineSpread)
                                                    |> Print.followedBy
                                                        parameterTypePrint
                                                )
                                            )
                                        )
                            }
                    )

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrints
                |> Print.lineSpreadListMapAndCombine
                    Print.lineSpread
    in
    Print.exactly "fun "
        |> Print.followedBy
            (Print.withIndentIncreasedBy 4
                (parameterPrints
                    |> Print.listMapAndIntersperseAndFlatten
                        (\parameterPrint -> parameterPrint)
                        (Print.spaceOrLinebreakIndented parametersLineSpread)
                )
            )
        |> Print.followedBy (Print.exactly " ->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionNotParenthesized
                            syntaxLambda.result
                        )
                )
            )


printFsharpExpressionIfElse :
    { condition : FsharpExpression
    , onTrue : FsharpExpression
    , onFalse : FsharpExpression
    }
    -> Print
printFsharpExpressionIfElse syntaxIfElse =
    let
        conditionPrint : Print
        conditionPrint =
            printFsharpExpressionNotParenthesized syntaxIfElse.condition

        conditionLineSpread : Print.LineSpread
        conditionLineSpread =
            conditionPrint |> Print.lineSpread
    in
    Print.exactly "if"
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented conditionLineSpread
                    |> Print.followedBy conditionPrint
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented conditionLineSpread)
        |> Print.followedBy (Print.exactly "then")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionNotParenthesized syntaxIfElse.onTrue)
                    |> Print.followedBy Print.linebreak
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "else")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionNotParenthesized syntaxIfElse.onFalse)
                )
            )


printFsharpExpressionMatchWith :
    { matched : FsharpExpression
    , case0 :
        { pattern : FsharpPattern
        , patternType : FsharpType
        , result : FsharpExpression
        }
    , case1Up :
        List
            { pattern : FsharpPattern
            , patternType : FsharpType
            , result : FsharpExpression
            }
    }
    -> Print
printFsharpExpressionMatchWith matchWith =
    let
        matchedPrint : Print
        matchedPrint =
            printFsharpExpressionNotParenthesized matchWith.matched

        matchedPrintLineSpread : Print.LineSpread
        matchedPrintLineSpread =
            matchedPrint |> Print.lineSpread
    in
    Print.exactly "match"
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented matchedPrintLineSpread
                    |> Print.followedBy matchedPrint
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented matchedPrintLineSpread)
        |> Print.followedBy (Print.exactly "with")
        |> Print.followedBy
            (Print.linebreakIndented
                |> Print.followedBy
                    ((matchWith.case0 :: matchWith.case1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\syntaxCase -> syntaxCase |> printFsharpExpressionMatchWithCase)
                            printLinebreakLinebreakIndented
                    )
            )


printFsharpExpressionMatchWithCase :
    { pattern : FsharpPattern
    , patternType : FsharpType
    , result : FsharpExpression
    }
    -> Print
printFsharpExpressionMatchWithCase branch =
    let
        patternPrint : Print
        patternPrint =
            printFsharpPatternNotParenthesized branch.pattern
    in
    Print.exactly "| "
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                patternPrint
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented
                (patternPrint |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly "->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionNotParenthesized branch.result)
                )
            )


printFsharpExpressionWithLetDeclarations :
    { declaration0 : FsharpLetDeclaration
    , declaration1Up : List FsharpLetDeclaration
    , result : FsharpExpression
    }
    -> Print
printFsharpExpressionWithLetDeclarations syntaxLetIn =
    let
        letDestructurings :
            List
                { pattern : FsharpPattern
                , patternType : FsharpType
                , expression : FsharpExpression
                }
        letDestructurings =
            (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            FsharpLetDestructuring letDestructuring ->
                                Just letDestructuring

                            FsharpLetDeclarationValueOrFunction _ ->
                                Nothing
                    )

        letValueOrFunctions :
            List
                { name : String
                , result : FsharpExpression
                , type_ : FsharpType
                }
        letValueOrFunctions =
            (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            FsharpLetDeclarationValueOrFunction letValueOrFunction ->
                                Just letValueOrFunction

                            FsharpLetDestructuring _ ->
                                Nothing
                    )

        ordered :
            { mostToLeastDependedOn :
                List (FsharpValueOrFunctionDependencyBucket FsharpLetDeclaration)
            }
        ordered =
            { mostToLeastDependedOn =
                letValueOrFunctions
                    |> fsharpValueOrFunctionDeclarationsGroupByDependencies
                    |> .mostToLeastDependedOn
                    |> List.map
                        (\fsharpValueOrFunctionDependencyBucket ->
                            case fsharpValueOrFunctionDependencyBucket of
                                FsharpValueOrFunctionDependencySingle fsharpValueOrFunction ->
                                    FsharpValueOrFunctionDependencySingle
                                        (FsharpLetDeclarationValueOrFunction fsharpValueOrFunction)

                                FsharpValueOrFunctionDependencyRecursiveBucket recursiveBucket ->
                                    FsharpValueOrFunctionDependencyRecursiveBucket
                                        (recursiveBucket
                                            |> List.map FsharpLetDeclarationValueOrFunction
                                        )
                        )
            }
                |> fsharpLetDeclarationsInsertFsharpLetDestructurings
                    letDestructurings
    in
    (ordered.mostToLeastDependedOn
        |> Print.listMapAndIntersperseAndFlatten
            (\dependencyGroup ->
                case dependencyGroup of
                    FsharpValueOrFunctionDependencySingle fsharpLetDeclaration ->
                        case fsharpLetDeclaration of
                            FsharpLetDestructuring letDestructuring ->
                                Print.exactly "let "
                                    |> Print.followedBy
                                        (letDestructuring |> printFsharpLetDestructuring)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy Print.linebreakIndented

                            FsharpLetDeclarationValueOrFunction letValueOrFunction ->
                                Print.exactly "let "
                                    |> Print.followedBy
                                        (letValueOrFunction |> printFsharpLocalLetValueOrFunctionDeclaration)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy Print.linebreakIndented

                    FsharpValueOrFunctionDependencyRecursiveBucket recursiveGroup ->
                        Print.exactly "let rec "
                            |> Print.followedBy
                                (recursiveGroup
                                    |> Print.listMapAndIntersperseAndFlatten
                                        (\fsharpLetDeclaration ->
                                            case fsharpLetDeclaration of
                                                FsharpLetDestructuring letDestructuring ->
                                                    (letDestructuring |> printFsharpLetDestructuring)
                                                        |> Print.followedBy Print.linebreakIndented

                                                FsharpLetDeclarationValueOrFunction letValueOrFunction ->
                                                    (letValueOrFunction |> printFsharpLocalLetValueOrFunctionDeclaration)
                                                        |> Print.followedBy Print.linebreakIndented
                                        )
                                        (Print.exactly "and ")
                                )
            )
            Print.empty
    )
        |> Print.followedBy
            (printFsharpExpressionNotParenthesized syntaxLetIn.result)


fsharpLetDeclarationsInsertFsharpLetDestructurings :
    List
        { pattern : FsharpPattern
        , patternType : FsharpType
        , expression : FsharpExpression
        }
    ->
        { mostToLeastDependedOn :
            List (FsharpValueOrFunctionDependencyBucket FsharpLetDeclaration)
        }
    ->
        { mostToLeastDependedOn :
            List (FsharpValueOrFunctionDependencyBucket FsharpLetDeclaration)
        }
fsharpLetDeclarationsInsertFsharpLetDestructurings fsharpLetDestructuringsToInsert existingLetDeclarations =
    fsharpLetDestructuringsToInsert
        |> List.foldl
            (\fsharpLetDestructuringToInsert soFar ->
                soFar
                    |> fsharpLetDeclarationsInsertFsharpLetDestructuring
                        fsharpLetDestructuringToInsert
            )
            existingLetDeclarations


fsharpLetDeclarationsInsertFsharpLetDestructuring :
    { pattern : FsharpPattern
    , patternType : FsharpType
    , expression : FsharpExpression
    }
    ->
        { mostToLeastDependedOn :
            List (FsharpValueOrFunctionDependencyBucket FsharpLetDeclaration)
        }
    ->
        { mostToLeastDependedOn :
            List (FsharpValueOrFunctionDependencyBucket FsharpLetDeclaration)
        }
fsharpLetDeclarationsInsertFsharpLetDestructuring fsharpLetDestructuringToInsert existingLetDeclarations =
    let
        variablesIntroducedInDestructuringPattern : FastSet.Set String
        variablesIntroducedInDestructuringPattern =
            fsharpLetDestructuringToInsert.pattern
                |> fsharpPatternContainedVariables

        withLetDestructuring :
            { destructuringHasBeenInserted : Bool
            , leastToMostDependedOn :
                List (FsharpValueOrFunctionDependencyBucket FsharpLetDeclaration)
            }
        withLetDestructuring =
            existingLetDeclarations.mostToLeastDependedOn
                |> List.foldl
                    (\existingLetDeclarationDependencyBucket soFar ->
                        if soFar.destructuringHasBeenInserted then
                            { destructuringHasBeenInserted = True
                            , leastToMostDependedOn =
                                existingLetDeclarationDependencyBucket
                                    :: soFar.leastToMostDependedOn
                            }

                        else
                            let
                                existingLetDeclarationUsedLocalReferences : FastSet.Set String
                                existingLetDeclarationUsedLocalReferences =
                                    case existingLetDeclarationDependencyBucket of
                                        FsharpValueOrFunctionDependencySingle existingLetDeclaration ->
                                            existingLetDeclaration
                                                |> fsharpLetDeclarationUsedLocalReferences

                                        FsharpValueOrFunctionDependencyRecursiveBucket recursiveBucketMembers ->
                                            recursiveBucketMembers
                                                |> listMapToFastSetsAndUnify
                                                    fsharpLetDeclarationUsedLocalReferences
                            in
                            if fastSetsIntersect variablesIntroducedInDestructuringPattern existingLetDeclarationUsedLocalReferences then
                                { destructuringHasBeenInserted = True
                                , leastToMostDependedOn =
                                    existingLetDeclarationDependencyBucket
                                        :: FsharpValueOrFunctionDependencySingle
                                            (FsharpLetDestructuring fsharpLetDestructuringToInsert)
                                        :: soFar.leastToMostDependedOn
                                }

                            else
                                { destructuringHasBeenInserted = False
                                , leastToMostDependedOn =
                                    existingLetDeclarationDependencyBucket
                                        :: soFar.leastToMostDependedOn
                                }
                    )
                    { destructuringHasBeenInserted = False
                    , leastToMostDependedOn = []
                    }
    in
    { mostToLeastDependedOn =
        if withLetDestructuring.destructuringHasBeenInserted then
            withLetDestructuring.leastToMostDependedOn |> List.reverse

        else
            FsharpValueOrFunctionDependencySingle
                (FsharpLetDestructuring fsharpLetDestructuringToInsert)
                :: withLetDestructuring.leastToMostDependedOn
                |> List.reverse
    }


fastSetsIntersect : FastSet.Set comparable -> FastSet.Set comparable -> Bool
fastSetsIntersect aSet bSet =
    aSet
        |> fastSetAny
            (\aElement ->
                bSet |> FastSet.member aElement
            )


fastSetAny : (a -> Bool) -> FastSet.Set a -> Bool
fastSetAny isFound fastSet =
    fastSet
        |> FastSet.stoppableFoldl
            (\element _ ->
                if element |> isFound then
                    FastDict.Stop True

                else
                    FastDict.Continue False
            )
            False


fsharpLetDeclarationUsedLocalReferences : FsharpLetDeclaration -> FastSet.Set String
fsharpLetDeclarationUsedLocalReferences fsharpLetDeclaration =
    case fsharpLetDeclaration of
        FsharpLetDestructuring fsharpLetDestructuring ->
            fsharpLetDestructuring.expression
                |> fsharpExpressionContainedLocalReferences

        FsharpLetDeclarationValueOrFunction fsharpLetValueOrFunction ->
            fsharpLetValueOrFunction.result
                |> fsharpExpressionContainedLocalReferences


fsharpPatternContainedVariables : FsharpPattern -> FastSet.Set String
fsharpPatternContainedVariables fsharpPattern =
    -- IGNORE TCO
    case fsharpPattern of
        FsharpPatternIgnore ->
            FastSet.empty

        FsharpPatternInt _ ->
            FastSet.empty

        FsharpPatternChar _ ->
            FastSet.empty

        FsharpPatternString _ ->
            FastSet.empty

        FsharpPatternVariable variable ->
            FastSet.singleton variable

        FsharpPatternAs fsharpPatternAs ->
            FastSet.insert fsharpPatternAs.variable
                (fsharpPatternAs.pattern |> fsharpPatternContainedVariables)

        FsharpPatternListExact elementPatterns ->
            elementPatterns
                |> listMapToFastSetsAndUnify fsharpPatternContainedVariables

        FsharpPatternTuple partPatterns ->
            FastSet.union
                (partPatterns.part0 |> fsharpPatternContainedVariables)
                (FastSet.union
                    (partPatterns.part1 |> fsharpPatternContainedVariables)
                    (partPatterns.part2Up
                        |> listMapToFastSetsAndUnify fsharpPatternContainedVariables
                    )
                )

        FsharpPatternListCons patternListCons ->
            FastSet.union
                (patternListCons.head |> fsharpPatternContainedVariables)
                (patternListCons.tail |> fsharpPatternContainedVariables)

        FsharpPatternVariant patternVariant ->
            patternVariant.values
                |> listMapToFastSetsAndUnify fsharpPatternContainedVariables

        FsharpPatternRecordInexhaustive recordPatternInexhaustiveFieldNames ->
            recordPatternInexhaustiveFieldNames
                |> FastDict.values
                |> listMapToFastSetsAndUnify fsharpPatternContainedVariables


printFsharpLetDestructuring :
    { pattern : FsharpPattern
    , patternType : FsharpType
    , expression : FsharpExpression
    }
    -> Print
printFsharpLetDestructuring letDestructuring =
    let
        patternTypePrint : Print
        patternTypePrint =
            letDestructuring.patternType
                |> printFsharpTypeParenthesizedIfSpaceSeparated
    in
    Print.withIndentAtNextMultipleOf4
        (printParenthesized
            { opening = "("
            , closing = ")"
            , inner =
                letDestructuring.pattern
                    |> printFsharpPatternNotParenthesized
                    |> Print.followedBy
                        (Print.exactly ":")
                    |> Print.followedBy
                        (Print.withIndentAtNextMultipleOf4
                            (Print.spaceOrLinebreakIndented
                                (patternTypePrint |> Print.lineSpread)
                                |> Print.followedBy patternTypePrint
                            )
                        )
            }
            |> Print.followedBy (Print.exactly " =")
            |> Print.followedBy
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionNotParenthesized letDestructuring.expression)
                )
        )


fsharpTypeUnit : FsharpType
fsharpTypeUnit =
    FsharpTypeConstruct
        { moduleOrigin = Nothing
        , name = "unit"
        , arguments = []
        }


{-| Print value/function declarations into
an fsharp module called `Elm` in the global namespace that exposes all members.
Will also add some internal wrapper declarations.
-}
fsharpDeclarationsToModuleString :
    { valuesAndFunctions :
        FastDict.Dict
            String
            { result : FsharpExpression
            , type_ : FsharpType
            }
    , typeAliases :
        FastDict.Dict
            String
            { parameters : List String
            , type_ : FsharpType
            }
    , recordTypes : FastSet.Set (List String)
    , choiceTypes :
        FastDict.Dict
            String
            { parameters : List String
            , variants : FastDict.Dict String (Maybe FsharpType)
            }
    }
    -> String
fsharpDeclarationsToModuleString fsharpDeclarations =
    let
        valueAndFunctionDeclarationsOrdered :
            { mostToLeastDependedOn :
                List
                    (FsharpValueOrFunctionDependencyBucket
                        { name : String
                        , result : FsharpExpression
                        , type_ : FsharpType
                        }
                    )
            }
        valueAndFunctionDeclarationsOrdered =
            fsharpDeclarations.valuesAndFunctions
                |> fastDictMapAndToList
                    (\name valueOrFunctionInfo ->
                        { name = name
                        , type_ = valueOrFunctionInfo.type_
                        , result = valueOrFunctionInfo.result
                        }
                    )
                |> fsharpValueOrFunctionDeclarationsGroupByDependencies

        typeDeclarationsOrdered :
            { mostToLeastDependedOn :
                List
                    (FsharpValueOrFunctionDependencyBucket
                        FsharpChoiceTypeOrTypeAliasDeclaration
                    )
            }
        typeDeclarationsOrdered =
            fsharpTypeDeclarationsGroupByDependencies
                { typeAliases =
                    fsharpDeclarations.typeAliases
                        |> fastDictMapAndToList
                            (\name info ->
                                { name = name
                                , parameters = info.parameters
                                , type_ = info.type_
                                }
                            )
                , enums =
                    fsharpDeclarations.choiceTypes
                        |> fastDictMapAndToList
                            (\name info ->
                                { name = name
                                , parameters = info.parameters
                                , variants = info.variants
                                }
                            )
                }
    in
    """namespace global

open System
open System.Runtime.CompilerServices

module Elm =
"""
        ++ defaultDeclarations
        ++ """

    """
        ++ (Print.withIndentAtNextMultipleOf4
                (fsharpDeclarations.recordTypes
                    |> FastSet.foldr
                        (\recordTypeFields soFar ->
                            printFsharpRecordTypeDeclaration recordTypeFields
                                :: soFar
                        )
                        []
                    |> Print.listIntersperseAndFlatten
                        (Print.linebreak
                            |> Print.followedBy Print.linebreakIndented
                        )
                )
                |> Print.toString
           )
        ++ """

    """
        ++ (Print.withIndentAtNextMultipleOf4
                (typeDeclarationsOrdered.mostToLeastDependedOn
                    |> Print.listMapAndIntersperseAndFlatten
                        (\typeAliasDeclarationGroup ->
                            case typeAliasDeclarationGroup of
                                FsharpValueOrFunctionDependencySingle single ->
                                    case single of
                                        FsharpChoiceTypeDeclaration fsharpChoiceTypeDeclaration ->
                                            Print.exactly "[<Struct>]"
                                                |> Print.followedBy Print.linebreakIndented
                                                |> Print.followedBy (Print.exactly "type ")
                                                |> Print.followedBy
                                                    (printFsharpChoiceTypeDeclaration
                                                        fsharpChoiceTypeDeclaration
                                                    )

                                        FsharpTypeAliasDeclaration aliasDeclaration ->
                                            Print.exactly "type "
                                                |> Print.followedBy
                                                    (printFsharpTypeAliasDeclaration aliasDeclaration)

                                FsharpValueOrFunctionDependencyRecursiveBucket recursiveBucket ->
                                    case recursiveBucket of
                                        [] ->
                                            Print.empty

                                        recursiveBucketMember0 :: recursiveBucketMember1Up ->
                                            (case recursiveBucketMember0 of
                                                FsharpChoiceTypeDeclaration fsharpChoiceTypeDeclaration ->
                                                    Print.exactly "type "
                                                        |> Print.followedBy
                                                            (printFsharpChoiceTypeDeclaration
                                                                fsharpChoiceTypeDeclaration
                                                            )

                                                FsharpTypeAliasDeclaration aliasDeclaration ->
                                                    Print.exactly "type "
                                                        |> Print.followedBy
                                                            (printFsharpTypeAliasDeclaration aliasDeclaration)
                                            )
                                                |> Print.followedBy
                                                    (recursiveBucketMember1Up
                                                        |> Print.listMapAndIntersperseAndFlatten
                                                            (\typeDeclaration ->
                                                                Print.linebreak
                                                                    |> Print.followedBy Print.linebreakIndented
                                                                    |> Print.followedBy
                                                                        (case typeDeclaration of
                                                                            FsharpChoiceTypeDeclaration enumDeclaration ->
                                                                                Print.exactly "and "
                                                                                    |> Print.followedBy
                                                                                        (printFsharpChoiceTypeDeclaration enumDeclaration)

                                                                            FsharpTypeAliasDeclaration aliasDeclaration ->
                                                                                Print.exactly "and "
                                                                                    |> Print.followedBy
                                                                                        (printFsharpTypeAliasDeclaration aliasDeclaration)
                                                                        )
                                                            )
                                                            Print.empty
                                                    )
                        )
                        (Print.linebreak
                            |> Print.followedBy Print.linebreakIndented
                        )
                )
                |> Print.toString
           )
        ++ """


    """
        ++ (Print.withIndentAtNextMultipleOf4
                (valueAndFunctionDeclarationsOrdered.mostToLeastDependedOn
                    |> Print.listMapAndIntersperseAndFlatten
                        (\dependencyGroup ->
                            case dependencyGroup of
                                FsharpValueOrFunctionDependencySingle fsharpValueOrFunction ->
                                    Print.exactly "let "
                                        |> Print.followedBy
                                            (fsharpValueOrFunction |> printFsharpValueOrFunctionDeclaration)
                                        |> Print.followedBy Print.linebreak
                                        |> Print.followedBy Print.linebreakIndented

                                FsharpValueOrFunctionDependencyRecursiveBucket recursiveGroup ->
                                    case recursiveGroup of
                                        [] ->
                                            Print.empty

                                        fsharpValueOrFunctionDeclaration0 :: fsharpValueOrFunctionDeclaration1Up ->
                                            Print.exactly "let rec "
                                                |> Print.followedBy
                                                    ((fsharpValueOrFunctionDeclaration0 |> printFsharpValueOrFunctionDeclaration)
                                                        |> Print.followedBy Print.linebreak
                                                        |> Print.followedBy Print.linebreakIndented
                                                    )
                                                |> Print.followedBy
                                                    (fsharpValueOrFunctionDeclaration1Up
                                                        |> Print.listMapAndIntersperseAndFlatten
                                                            (\letValueOrFunction ->
                                                                Print.exactly "and "
                                                                    |> Print.followedBy
                                                                        (letValueOrFunction |> printFsharpValueOrFunctionDeclaration)
                                                                    |> Print.followedBy Print.linebreak
                                                                    |> Print.followedBy Print.linebreakIndented
                                                            )
                                                            Print.empty
                                                    )
                        )
                        Print.empty
                )
                |> Print.toString
           )
        ++ """
"""


defaultDeclarations : String
defaultDeclarations =
    """
    let inline Basics_always (result: 'result) (_: '_ignored) : 'result = result

    let inline Basics_eq (a: 'a) (b: 'a) : bool = a = b
    let inline Basics_neq (a: 'a) (b: 'a) : bool = a <> b
    let inline Basics_flt (a: float) (b: float) : bool = a < b
    let inline Basics_ilt (a: int64) (b: int64) : bool = a < b
    let inline Basics_fle (a: float) (b: float) : bool = a <= b
    let inline Basics_ile (a: int64) (b: int64) : bool = a <= b
    let inline Basics_fgt (a: float) (b: float) : bool = a > b
    let inline Basics_igt (a: int64) (b: int64) : bool = a > b
    let inline Basics_fge (a: float) (b: float) : bool = a >= b
    let inline Basics_ige (a: int64) (b: int64) : bool = a >= b

    [<Struct>]
    type Basics_Order =
        | LT = -1
        | EQ = 0
        | GT = 1

    let inline Basics_compare (a: 'a) (b: 'a) : Basics_Order =
        let comparisonMagnitude = compare a b

        if comparisonMagnitude = 0 then Basics_Order.EQ
        else if comparisonMagnitude < 0 then Basics_Order.LT
        else Basics_Order.GT

    let inline Basics_ceiling (n: float) : int64 =
        int64 (System.Math.Ceiling(n))
    let inline Basics_floor (n: float) : int64 =
        int64 (System.Math.Floor(n))
    let inline Basics_round (n: float) : int64 =
        int64 (System.Math.Round(n))
    let inline Basics_fabs (n: float) : float = System.Double.Abs(n)
    let inline Basics_iabs (n: int64) : int64 = System.Int64.Abs(n)
    let inline Basics_fnegate (n: float) : float = -n
    let inline Basics_inegate (n: int64) : int64 = -n
    let inline Basics_fadd (a: float) (b: float) : float = a + b
    let inline Basics_iadd (a: int64) (b: int64) : int64 = a + b
    let inline Basics_fsub (a: float) (b: float) : float = a - b
    let inline Basics_isub (a: int64) (b: int64) : int64 = a - b
    let inline Basics_fmul (a: float) (b: float) : float = a * b
    let inline Basics_imul (a: int64) (b: int64) : int64 = a * b
    let inline Basics_fdiv (a: float) (b: float) : float = a / b
    let inline Basics_idiv (a: int64) (b: int64) : int64 = a / b
    let inline Basics_remainderBy (divisor: int64) (toDivide: int64) : int64 =
        toDivide % divisor

    let Basics_modBy (divisor: int64) (toDivide: int64) : int64 =
        let remainder = toDivide % divisor

        if
            (remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0)
        then
            remainder + toDivide

        else
            remainder

    let inline Basics_fpow (a: float) (b: float) : float = a ** b
    let inline Basics_ipow (a: int64) (b: int64) : int64 = int64 (float a ** float b)
    let inline Basics_iclamp (minimum: int64) (maximum: int64) (n: int64) : int64 =
        System.Math.Clamp(value=n, min=minimum, max=maximum)
    let inline Basics_fclamp (minimum: float) (maximum: float) (n: float) : float =
        System.Math.Clamp(value=n, min=minimum, max=maximum)
    let inline Basics_logBase (newBase: float) (n: float) : float =
        System.Math.Log(n, newBase=newBase)
    
    let inline Basics_atan2 (y: float) (x: float) : float =
        System.Double.Atan2(y, x)
    let inline Basics_radians (radians: float) : float =
        radians
    let inline Basics_degrees (angleInDegrees: float) : float =
        (angleInDegrees * System.Math.PI) / 180.0
    let inline Basics_turns (angleInTurns: float) : float =
        (System.Math.PI * 2.0) * angleInTurns
    let Basics_fromPolar (struct( radius: float, theta: float )) : struct( float * float ) =
        struct(
          radius * (System.Double.Cos(theta))
        , radius * (System.Double.Sin(theta))
        )
    let Basics_toPolar (struct( x: float, y: float )): struct( float * float ) =
        struct (
          System.Double.Sqrt((x * x) + (y * y))
        , System.Double.Atan2(y, x)
        )

    let inline Bitwise_shiftLeftBy (bitPositionsToShiftBy: int64) (n: int64) : int64 =
        n <<< int32 bitPositionsToShiftBy
    let inline Bitwise_shiftRightBy (bitPositionsToShiftBy: int64) (n: int64) : int64 =
        n >>> int32 bitPositionsToShiftBy
    let inline Bitwise_shiftRightZfBy (bitPositionsToShiftBy: int64) (n: int64) : int64 =
        int64 (int64 n >>> int32 bitPositionsToShiftBy);

    let inline Basics_and (a: bool) (b: bool) : bool = a && b
    let inline Basics_or (a: bool) (b: bool) : bool = a || b

    type Result_Result<'error, 'value> =
        Result<'value, 'error>

    type Basics_Never =
        | JustOneMore of Basics_Never
    let rec Basics_never (JustOneMore ever: Basics_Never) =
        Basics_never ever
 
    let inline Char_isOctDigit (ch: char) : bool =
        let code = int ch

        code <= 0x37 && 0x30 <= code
    
    [<CustomEquality; CustomComparison>]
    type StringRope =
        | StringRopeOne of string
        | StringRopeAppend of StringRope * StringRope
    
        static member toString (this: StringRope) : string =
            match this with
            | StringRopeOne content -> content
            | StringRopeAppend (fullLeftRope, fullRightRope) ->
                let mutableBuilder = System.Text.StringBuilder()
                let mutable stringRopeToMatchNext = fullLeftRope
                let mutable shouldKeepGoing = true
                let mutableRemainingRightStringRopes: System.Collections.Generic.Stack<StringRope> = System.Collections.Generic.Stack()
                mutableRemainingRightStringRopes.Push(fullRightRope)
                while (shouldKeepGoing) do
                    match stringRopeToMatchNext with
                    | StringRopeOne segment ->
                        let _ = mutableBuilder.Append(segment)
                        if mutableRemainingRightStringRopes.Count = 0 then
                            shouldKeepGoing <- false
                        else
                            stringRopeToMatchNext <-
                                mutableRemainingRightStringRopes.Pop()
                    | StringRopeAppend (left, right) ->
                        stringRopeToMatchNext <- left
                        mutableRemainingRightStringRopes.Push(right)
                done
                mutableBuilder.ToString()
        override x.GetHashCode() =
            hash (StringRope.toString(x))
        override x.Equals(other) =
            match other with
                | :? StringRope as otherStringRope ->
                    StringRope.toString(x) = StringRope.toString(otherStringRope)
                | _ -> false
        interface System.IComparable with
            member x.CompareTo(other) =
                match other with
                | :? StringRope as otherStringRope ->
                    (StringRope.toString(x)).CompareTo (StringRope.toString(otherStringRope))
                | _ -> -1
    
    let stringRopeEmpty: StringRope = StringRopeOne ""

    let rec String_isEmpty (stringToCheck: StringRope) : bool =
        match stringToCheck with
        | StringRopeOne string -> System.String.IsNullOrEmpty(string)
        | StringRopeAppend (left, right) ->
            String_isEmpty left && String_isEmpty right

    let inline String_length (str: StringRope) : int64 =
        String.length (StringRope.toString str)

    let inline String_repeat (repetitions: int64) (segment: StringRope) : StringRope =
        StringRopeOne (String.replicate (int repetitions) (StringRope.toString segment))

    let String_toList (string: StringRope) : List<char> =
        List.ofArray ((StringRope.toString string).ToCharArray())

    let inline String_fromList (chars: List<char>) : StringRope =
        StringRopeOne (new string (List.toArray chars))

    let inline String_contains (substringRope: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).Contains(StringRope.toString substringRope)

    let inline String_startsWith (start: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).StartsWith(StringRope.toString start)

    let inline String_endsWith (ending: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).EndsWith(StringRope.toString ending)

    let inline String_any
        ([<InlineIfLambda>] charIsNeedle: char -> bool)
        (string: StringRope)
        : bool =
        // can be optimized
        String.exists charIsNeedle (StringRope.toString string)

    let inline String_all
        ([<InlineIfLambda>] charIsExpected: char -> bool)
        (string: StringRope)
        : bool =
        // can be optimized
        String.forall charIsExpected (StringRope.toString string)

    let inline String_map
        ([<InlineIfLambda>] charChange: char -> char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            (String.map charChange (StringRope.toString string))

    let inline String_filter
        ([<InlineIfLambda>] charShouldBeKept: char -> bool)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            (String.filter charShouldBeKept (StringRope.toString string))

    let inline String_foldl
        ([<InlineIfLambda>] reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: StringRope)
        : 'folded =
        Seq.fold (fun soFar char -> reduce char soFar) initialFolded
            (StringRope.toString string)

    let inline String_foldr
        ([<InlineIfLambda>] reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: StringRope)
        : 'folded =
        Seq.foldBack reduce
            (StringRope.toString string)
            initialFolded

    let inline String_trim (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).Trim())
    let inline String_trimLeft (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).TrimStart())
    let inline String_trimRight (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).TrimEnd())

    let String_right (takenElementCount: int64) (stringRope: StringRope) : StringRope =
        let string: string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                String.length string - int takenElementCount,
                int takenElementCount
            ))

    let inline String_left (skippedElementCount: int64) (string: StringRope) : StringRope = 
        StringRopeOne
            ((StringRope.toString string).Substring(0, int skippedElementCount))
    
    let String_dropRight (skippedElementCount: int64) (stringRope: StringRope) : StringRope =
        let string: string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                0,
                String.length string - int skippedElementCount
            ))

    let String_dropLeft (skippedElementCount: int64) (stringRope: StringRope) : StringRope =
        let string: string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                int skippedElementCount,
                String.length string - int skippedElementCount
            ))

    let inline String_append (early: StringRope) (late: StringRope) : StringRope =
        StringRopeAppend (early, late)
    let inline String_fromChar (char: char) : StringRope =
        StringRopeOne (string char)

    let inline String_cons (newHeadChar: char) (late: StringRope) : StringRope =
        StringRopeAppend (StringRopeOne (string newHeadChar), late)
    
    let String_uncons (stringRope: StringRope) : option<struct( char * StringRope )> =
        let string: string = StringRope.toString stringRope
        if System.String.IsNullOrEmpty(string) then
            None
        else
            Some (struct( string[0], StringRopeOne(string[1..]) ))

    let String_split (separator: StringRope) (string: StringRope) : List<StringRope> =
        // can be optimized
        List.ofArray
            (Array.map (fun segment -> StringRopeOne segment)
                ((StringRope.toString string).Split(StringRope.toString separator))
            )

    let String_lines (string: StringRope) : List<StringRope> =
        // can be optimized
        List.ofArray (
            (Array.map (fun line -> StringRopeOne line)
                ((StringRope.toString string)
                    .Replace("\\r\\n", "\\n")
                    .Split("\\n")
                )
            )
        )

    let String_reverse (string: StringRope) : StringRope =
        StringRopeOne
            (new string (Array.rev ((StringRope.toString string).ToCharArray())))

    let inline String_replace
        (toReplace: StringRope)
        (replacement: StringRope)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).Replace(
                StringRope.toString toReplace,
                StringRope.toString replacement
            ))

    let inline String_toUpper (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).ToUpperInvariant())
    let inline String_toLower (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).ToLowerInvariant())

    let String_join (separator: StringRope) (strings: List<StringRope>) : StringRope =
        // can be optimized
        StringRopeOne
            (String.concat
                (StringRope.toString separator)
                (List.map StringRope.toString strings)
            )
    let String_concat (strings: List<StringRope>) : StringRope =
        // can be optimized
        StringRopeOne
            (System.String.Concat(List.map StringRope.toString strings))

    let inline String_padLeft
        (newMinimumLength: int64)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).PadLeft(int newMinimumLength, padding))

    let inline String_padRight
        (newMinimumLength: int64)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).PadRight(int newMinimumLength, padding))
    
    let inline String_fromFloat (n: float) : StringRope =
        StringRopeOne (string n)
    let inline String_fromInt (n: int64) : StringRope =
        StringRopeOne (string n)

    let String_toInt (string: StringRope) : option<int64> =
        let (success, num) = System.Int64.TryParse (StringRope.toString string)

        if success then Some num else None

    let String_toFloat (string: StringRope) : option<float> =
        let (success, num) = System.Double.TryParse (StringRope.toString string)

        if success then Some num else None

    let String_slice
        (startInclusivePossiblyNegative: int64)
        (endExclusivePossiblyNegative: int64)
        (stringRope: StringRope)
        : StringRope =
        let string = StringRope.toString stringRope
        let realStartIndex: int =
            if (startInclusivePossiblyNegative < 0L) then
                max
                    0
                    (int startInclusivePossiblyNegative + String.length string)
            else
                int startInclusivePossiblyNegative
        let realEndIndexExclusive: int =
            if (endExclusivePossiblyNegative < 0L) then
                max
                    0
                    (int endExclusivePossiblyNegative + String.length string)
            else
                min
                    (int endExclusivePossiblyNegative)
                    (String.length string)

        if (realStartIndex >= realEndIndexExclusive) then
            stringRopeEmpty
        else
            StringRopeOne
                (string.Substring(
                    realStartIndex,
                    realEndIndexExclusive - realStartIndex
                ))
    
    let inline List_length (list: List<'a>) : int64 =
        List.length list
    
    let inline List_tail (list: List<'a>) : option<List<'a>> =
        match list with
        | [] -> None
        | head :: tail ->
            Some tail

    let inline List_member (needle: 'a) (list: List<'a>) : bool =
        List.contains needle list
    
    let List_minimum (list: List<'a>): option<'a> =
        match list with
        | [] -> None
        | _ :: _ -> Some (List.min list)
    
    let List_maximum (list: List<'a>): option<'a> =
        match list with
        | [] -> None
        | _ :: _ -> Some (List.max list)

    let List_fproduct (list: List<float>) : float =
        List.fold (*) 1.0 list
    let List_iproduct (list: List<int64>) : int64 =
        List.fold (*) 1L list

    let inline List_cons (newHead: 'a) (tail: List<'a>) : List<'a> =
        newHead :: tail
    
    let inline List_repeat (repetitions: int64) (element: 'a) : List<'a> =
        List.replicate (int repetitions) element
    
    let inline List_take (elementCountFromStart: int64) (list: List<'a>) : List<'a> =
        List.truncate (int elementCountFromStart) list
    
    let inline List_drop (skippedElementCountFromStart: int64) (list: List<'a>) : List<'a> =
        try List.skip (int skippedElementCountFromStart) list with
        | _ -> []

    let inline List_sortWith
        ([<InlineIfLambda>] elementCompare: 'a -> 'a -> Basics_Order)
        (list: List<'a>)
        : List<'a> =
        List.sortWith
            (fun a b -> int (elementCompare a b))
            list

    let List_intersperse (sep: 'a) (list: List<'a>) =
        match list with
        | [] -> []
        | listHead :: listTail ->
            List.foldBack
                (fun x soFar -> x :: sep :: soFar)
                listTail
                [ listHead ]

    let inline List_foldl
        ([<InlineIfLambda>] reduce: 'a -> 'state -> 'state)
        (initialState: 'state)
        (list: List<'a>)
        : 'state =
        List.fold
            (fun soFar element -> reduce element soFar)
            initialState
            list

    let inline List_foldr
        ([<InlineIfLambda>] reduce: 'a -> 'state -> 'state)
        (initialState: 'state)
        (list: List<'a>)
        : 'state =
        List.foldBack reduce list initialState

    let inline List_range (startFloat: int64) (endFloat: int64) : List<int64> =
        [ startFloat..endFloat ]
    
    let rec List_map4_into_reverse
        (combinedSoFarReverse: List<'combined>)
        (combine: 'a -> 'b -> 'c -> 'd -> 'combined)
        (aList: List<'a>)
        (bList: List<'b>)
        (cList: List<'c>)
        (dList: List<'d>)
        : List<'combined> =
        // optimization possibility: construct into an array first
        match aList with
        | [] -> List.rev combinedSoFarReverse
        | aHead :: aTail ->
            match bList with
            | [] -> List.rev combinedSoFarReverse
            | bHead :: bTail ->
                match cList with
                | [] -> List.rev combinedSoFarReverse
                | cHead :: cTail ->
                    match dList with
                    | [] -> List.rev combinedSoFarReverse
                    | dHead :: dTail ->
                        List_map4_into_reverse
                            (combine aHead bHead cHead dHead
                                :: combinedSoFarReverse
                            )
                            combine
                            aTail
                            bTail
                            cTail
                            dTail
    
    let inline List_map4
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'c -> 'd -> 'combined)
        (aList: List<'a>)
        (bList: List<'b>)
        (cList: List<'c>)
        (dList: List<'d>)
        : List<'combined> =
        List_map4_into_reverse [] combine aList bList cList dList
    
    let rec List_map5_into_reverse
        (combinedSoFarReverse: List<'combined>)
        (combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined)
        (aList: List<'a>)
        (bList: List<'b>)
        (cList: List<'c>)
        (dList: List<'d>)
        (eList: List<'e>)
        : List<'combined> =
        // optimization possibility: construct into an array first
        match aList with
        | [] -> List.rev combinedSoFarReverse
        | aHead :: aTail ->
            match bList with
            | [] -> List.rev combinedSoFarReverse
            | bHead :: bTail ->
                match cList with
                | [] -> List.rev combinedSoFarReverse
                | cHead :: cTail ->
                    match dList with
                    | [] -> List.rev combinedSoFarReverse
                    | dHead :: dTail ->
                        match eList with
                        | [] -> List.rev combinedSoFarReverse
                        | eHead :: eTail ->
                            List_map5_into_reverse
                                (combine aHead bHead cHead dHead eHead
                                    :: combinedSoFarReverse
                                )
                                combine
                                aTail
                                bTail
                                cTail
                                dTail
                                eTail
    
    let inline List_map5
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined)
        (aList: List<'a>)
        (bList: List<'b>)
        (cList: List<'c>)
        (dList: List<'d>)
        (eList: List<'e>)
        : List<'combined> =
        List_map5_into_reverse [] combine aList bList cList dList eList
    
    let inline Dict_size (dict: Map<'key, 'value>) : int64 =
        Map.count dict

    let inline Dict_singleton (key: 'key) (value: 'value) : Map<'key, 'value> =
        Map [ (key, value) ]
    
    let inline Dict_toList (dict: Map<'key, 'value>) : List<struct('key * 'value)> =
        Map.foldBack
            (fun key value soFar -> (struct( key, value )) :: soFar)
            dict
            []
    let inline Dict_fromList (keyValuePairs: List<struct('key * 'value)>) : Map<'key, 'value> =
        List.fold
            (fun soFar (struct( key, value )) ->
                Map.add key value soFar
            )
            Map.empty
            keyValuePairs

    let inline Dict_foldr
        ([<InlineIfLambda>] reduce: 'key -> 'value -> 'state -> 'state)
        (initialState: 'state)
        (dict: Map<'key, 'value>)
        : 'state =
        Map.foldBack reduce dict initialState

    let inline Dict_foldl
        ([<InlineIfLambda>] reduce: 'key -> 'value -> 'state -> 'state)
        (initialState: 'state)
        (dict: Map<'key, 'value>)
        : 'state =
        Map.fold (fun soFar k v -> reduce k v soFar) initialState dict

    let inline Dict_keys (dict: Map<'key, 'value>) : List<'key> =
        Seq.toList (Map.keys dict)

    let inline Dict_values (dict: Map<'key, 'value>) : List<'value> =
        Seq.toList (Map.values dict)

    let Dict_diff
        (baseDict: Map<'key, 'a>)
        (dictWithKeysToRemove: Map<'key, 'b>)
        : Map<'key, 'a> =
        Map.fold
            (fun soFar k _ -> Map.remove k soFar)
            baseDict
            dictWithKeysToRemove

    let Dict_union
        (aDict: Map<'key, 'a>)
        (bDict: Map<'key, 'a>)
        : Map<'key, 'a> =
        Map.fold (fun soFar k v -> Map.add k v soFar) bDict aDict
    
    let Dict_intersect (aDict: Map<'comparable, 'v>) (bDict: Map<'comparable, 'v>) =
        Map.filter (fun key _ -> Map.containsKey key bDict) aDict

    let Dict_merge
        (leftStep: ('comparable -> 'aValue -> 'result -> 'result))
        (bothStep: ('comparable -> 'aValue -> 'bValue -> 'result -> 'result))
        (rightStep: ('comparable -> 'bValue -> 'result -> 'result))
        (leftDict: Map<'comparable, 'aValue>)
        (rightDict: Map<'comparable, 'bValue>)
        (initialResult: 'result)
        : 'result =
        // can be optimized using ValueTuple
        let rec stepState ( list: List<( 'comparable * 'aValue )>, result: 'result ) (rKey: 'comparable) (rValue: 'bValue) =
            match list with
            | [] ->
                ( list, rightStep rKey rValue result )

            | ( lKey, lValue ) :: rest ->
                if lKey < rKey then
                    stepState ( rest, leftStep lKey lValue result ) rKey rValue

                else if lKey > rKey then
                    ( list, rightStep rKey rValue result )

                else
                    ( rest, bothStep lKey lValue rValue result )

        let ( leftovers: List<( 'comparable * 'aValue )>, intermediateResult: 'result ) =
            Map.fold stepState ( Map.toList leftDict, initialResult ) rightDict
        
        List.fold (fun result ( k, v ) -> leftStep k v result)
            intermediateResult
            leftovers

    let inline Set_size (set: Set<'element>) : int64 =
        Set.count set

    let inline Set_foldr
        ([<InlineIfLambda>] reduce: 'key -> 'state -> 'state)
        (initialState: 'state)
        (set: Set<'key>)
        : 'state =
        Set.foldBack reduce set initialState

    let inline Set_foldl
        ([<InlineIfLambda>] reduce: 'key -> 'state -> 'state)
        (initialState: 'state)
        (set: Set<'key>)
        : 'state =
        Set.fold (fun soFar k -> reduce k soFar) initialState set
    
    let inline Array_length (array: array<'a>) : int64 =
        Array.length array
    let Array_get (index: int64) (array: array<'element>) : option<'element> =
        Array.tryItem (int index) array
    let inline Array_initialize
        (count: int64)
        ([<InlineIfLambda>] indexToElement: int64 -> 'element)
        : array<'element> =
        Array.init (max 0 (int count)) (fun index -> indexToElement index)
    let inline Array_repeat (count: int64) (element: 'element) : array<'element> =
        Array.replicate (max 0 (int count)) element
    let Array_set (index: int64) (replacementElement: 'element) (array: array<'element>) : array<'element> =
        if index < 0 then
            array
        else if index >= Array.length array then
            array
        else
            Array.updateAt (int index) replacementElement array
    let Array_push (newLastElement: 'element) (array: array<'element>) : array<'element> =
        Array.append array [| newLastElement |]
    let inline Array_indexedMap
        ([<InlineIfLambda>] elementChange: int64 -> 'a -> 'b)
        (array: array<'a>)
        : array<'b> =
        Array.mapi (fun index element -> elementChange index element) array
    let Array_toIndexedList (array: array<'a>) : List<struct( int64 * 'a )> =
        (Array.foldBack
            (fun (element: 'a) (soFar: {| Index: int64; List: List<struct( int64 * 'a )> |}) ->
                {| Index = soFar.Index - 1L
                ;  List = (struct( soFar.Index, element )) :: soFar.List
                |}
            )
            array
            {| Index = int64 (Array.length array - 1)
            ;  List = []
            |}
        ).List
    let inline Array_foldl
        ([<InlineIfLambda>] reduce: 'a -> 'state -> 'state)
        (initialState: 'state)
        (array: array<'a>)
        : 'state =
        Array.fold (fun state element -> reduce element state) initialState array
    let inline Array_foldr
        ([<InlineIfLambda>] reduce: 'a -> 'state -> 'state)
        (initialState: 'state)
        (array: array<'a>)
        : 'state =
        Array.foldBack reduce array initialState
    let Array_slice
        (startInclusivePossiblyNegative: int64)
        (endExclusivePossiblyNegative: int64)
        (array: array<'a>)
        : array<'a> =
        let realStartIndex: int =
            if (startInclusivePossiblyNegative < 0L) then
                max
                    0
                    (int startInclusivePossiblyNegative + Array.length array)
            else
                int startInclusivePossiblyNegative
        let realEndIndexExclusive: int =
            if (endExclusivePossiblyNegative < 0L) then
                max
                    0
                    (int endExclusivePossiblyNegative + Array.length array)
            else
                min
                    (int endExclusivePossiblyNegative)
                    (Array.length array)

        if (realStartIndex >= realEndIndexExclusive) then
            Array.empty
        else
            Array.sub
                array
                realStartIndex
                (realEndIndexExclusive - realStartIndex)
    
    
    let JsonEncode_null : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(null)
    let inline JsonEncode_bool (bool: bool) : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(bool)
    let inline JsonEncode_string (string: StringRope) : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(StringRope.toString string)
    let inline JsonEncode_int (int: int64) : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(int)
    let inline JsonEncode_float (float: float) : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(float)
    let inline JsonEncode_list
        ([<InlineIfLambda>] elementToValue: 'element -> System.Text.Json.Nodes.JsonNode)
        (elements: List<'element>)
        : System.Text.Json.Nodes.JsonNode =
        // can be optimized
        System.Text.Json.Nodes.JsonArray(Array.ofList (List.map elementToValue elements))
    let inline JsonEncode_array
        ([<InlineIfLambda>] elementToValue: 'element -> System.Text.Json.Nodes.JsonNode)
        (elements: array<'element>)
        : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonArray(Array.map elementToValue elements)
    let inline JsonEncode_set
        ([<InlineIfLambda>] elementToValue: 'element -> System.Text.Json.Nodes.JsonNode)
        (elements: Set<'element>)
        : System.Text.Json.Nodes.JsonNode =
        // can be optimized
        System.Text.Json.Nodes.JsonArray(Array.map elementToValue (Set.toArray elements))
    let inline JsonEncode_object
        (fields: List<struct( StringRope * System.Text.Json.Nodes.JsonNode )>)
        : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonObject(
            List.fold
                (fun soFar (struct( fieldName, fieldValue )) ->
                    Map.add (StringRope.toString fieldName)
                        fieldValue
                        soFar
                )
                Map.empty
                fields
        )
    let inline JsonEncode_dict
        ([<InlineIfLambda>] keyToString: 'key -> string)
        ([<InlineIfLambda>] valueToJson: 'value -> System.Text.Json.Nodes.JsonNode)
        (dict: Map<'key, 'value>)
        : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonObject
            (Map.fold
                (fun soFar key value ->
                    Map.add (keyToString key) (valueToJson value) soFar
                )
                Map.empty
                dict
            )
            
    
    let JsonEncode_encode (indentDepth: int64) (json: System.Text.Json.Nodes.JsonNode) : StringRope =
        let printOptions =
            System.Text.Json.JsonSerializerOptions()
        if (indentDepth <> 0) then
            printOptions.WriteIndented <- true
            printOptions.IndentSize <- int indentDepth
        
        StringRopeOne (json.ToJsonString(printOptions))
    
    type JsonDecode_Error =
        | JsonDecode_Field of (struct( StringRope * JsonDecode_Error ))
        | JsonDecode_Index of (struct( int64 * JsonDecode_Error ))
        | JsonDecode_OneOf of List<JsonDecode_Error>
        | JsonDecode_Failure of (struct( StringRope * System.Text.Json.Nodes.JsonNode ))
    type JsonDecode_Decoder<'value> =
        System.Text.Json.Nodes.JsonNode -> Result<'value, JsonDecode_Error>
    
    let inline JsonDecode_decodeValue (decoder: JsonDecode_Decoder<'value>) (value: System.Text.Json.Nodes.JsonNode) : Result<'value, JsonDecode_Error> =
        decoder value
    let inline JsonDecode_decodeString (decoder: JsonDecode_Decoder<'value>) (string: StringRope) : Result<'value, JsonDecode_Error> =
        try decoder (System.Text.Json.Nodes.JsonNode.Parse(StringRope.toString string)) with
        | :? System.Text.Json.JsonException ->
            Error(
                JsonDecode_Failure(
                    StringRopeOne "This is not valid JSON!",
                    System.Text.Json.Nodes.JsonValue.Create(StringRope.toString string)
                )
            )
    
    let inline JsonDecode_succeed (value: 'value) : JsonDecode_Decoder<'value> =
        fun _ -> Ok(value)
    let inline JsonDecode_fail (errorMessage: StringRope) : JsonDecode_Decoder<'value> =
        fun jsonDomNode ->
            Error(JsonDecode_Failure(errorMessage, jsonDomNode))
    let inline JsonDecode_map
        ([<InlineIfLambda>] valueChange: 'a -> 'b)
        (decoder: JsonDecode_Decoder<'a>)
        : JsonDecode_Decoder<'b> =
        fun jsonDomNode ->
            match decoder jsonDomNode with
            | Error(error) -> Error(error)
            | Ok(value) -> Ok(valueChange value)
    let JsonDecode_lazy
        (lazilyConstructDecoder: unit -> JsonDecode_Decoder<'value>)
        : JsonDecode_Decoder<'value> =
        fun json ->
            lazilyConstructDecoder () json
    let inline JsonDecode_andThen
        ([<InlineIfLambda>] decoderBasedOnValue: 'a -> JsonDecode_Decoder<'b>)
        (decoder: JsonDecode_Decoder<'a>)
        : JsonDecode_Decoder<'b> =
        fun json ->
            match decoder json with
            | Error(error) -> Error(error)
            | Ok(value) -> decoderBasedOnValue value json
    let inline JsonDecode_map2
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'combined)
        (aDecoder: JsonDecode_Decoder<'a>)
        (bDecoder: JsonDecode_Decoder<'b>)
        : JsonDecode_Decoder<'combined> =
        fun json ->
            match aDecoder json with
            | Error error -> Error error
            | Ok a ->
                match bDecoder json with
                | Error error -> Error error
                | Ok b -> Ok (combine a b)
    let inline JsonDecode_map3
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'c -> 'combined)
        (aDecoder: JsonDecode_Decoder<'a>)
        (bDecoder: JsonDecode_Decoder<'b>)
        (cDecoder: JsonDecode_Decoder<'c>)
        : JsonDecode_Decoder<'combined> =
        fun json ->
            match aDecoder json with
            | Error error -> Error error
            | Ok a ->
                match bDecoder json with
                | Error error -> Error error
                | Ok b ->
                    match cDecoder json with
                    | Error error -> Error error
                    | Ok c -> Ok (combine a b c)
    let inline JsonDecode_map4
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'c -> 'd -> 'combined)
        (aDecoder: JsonDecode_Decoder<'a>)
        (bDecoder: JsonDecode_Decoder<'b>)
        (cDecoder: JsonDecode_Decoder<'c>)
        (dDecoder: JsonDecode_Decoder<'d>)
        : JsonDecode_Decoder<'combined> =
        fun json ->
            match aDecoder json with
            | Error error -> Error error
            | Ok a ->
                match bDecoder json with
                | Error error -> Error error
                | Ok b ->
                    match cDecoder json with
                    | Error error -> Error error
                    | Ok c ->
                        match dDecoder json with
                        | Error error -> Error error
                        | Ok d -> Ok (combine a b c d)
    let inline JsonDecode_map5
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined)
        (aDecoder: JsonDecode_Decoder<'a>)
        (bDecoder: JsonDecode_Decoder<'b>)
        (cDecoder: JsonDecode_Decoder<'c>)
        (dDecoder: JsonDecode_Decoder<'d>)
        (eDecoder: JsonDecode_Decoder<'e>)
        : JsonDecode_Decoder<'combined> =
        fun json ->
            match aDecoder json with
            | Error error -> Error error
            | Ok a ->
                match bDecoder json with
                | Error error -> Error error
                | Ok b ->
                    match cDecoder json with
                    | Error error -> Error error
                    | Ok c ->
                        match dDecoder json with
                        | Error error -> Error error
                        | Ok d ->
                            match eDecoder json with
                            | Error error -> Error error
                            | Ok e -> Ok (combine a b c d e)
    let inline JsonDecode_map6
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'combined)
        (aDecoder: JsonDecode_Decoder<'a>)
        (bDecoder: JsonDecode_Decoder<'b>)
        (cDecoder: JsonDecode_Decoder<'c>)
        (dDecoder: JsonDecode_Decoder<'d>)
        (eDecoder: JsonDecode_Decoder<'e>)
        (fDecoder: JsonDecode_Decoder<'f>)
        : JsonDecode_Decoder<'combined> =
        fun json ->
            match aDecoder json with
            | Error error -> Error error
            | Ok a ->
                match bDecoder json with
                | Error error -> Error error
                | Ok b ->
                    match cDecoder json with
                    | Error error -> Error error
                    | Ok c ->
                        match dDecoder json with
                        | Error error -> Error error
                        | Ok d ->
                            match eDecoder json with
                            | Error error -> Error error
                            | Ok e ->
                                match fDecoder json with
                                | Error error -> Error error
                                | Ok f -> Ok (combine a b c d e f)
    let inline JsonDecode_map7
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'combined)
        (aDecoder: JsonDecode_Decoder<'a>)
        (bDecoder: JsonDecode_Decoder<'b>)
        (cDecoder: JsonDecode_Decoder<'c>)
        (dDecoder: JsonDecode_Decoder<'d>)
        (eDecoder: JsonDecode_Decoder<'e>)
        (fDecoder: JsonDecode_Decoder<'f>)
        (gDecoder: JsonDecode_Decoder<'g>)
        : JsonDecode_Decoder<'combined> =
        fun json ->
            match aDecoder json with
            | Error error -> Error error
            | Ok a ->
                match bDecoder json with
                | Error error -> Error error
                | Ok b ->
                    match cDecoder json with
                    | Error error -> Error error
                    | Ok c ->
                        match dDecoder json with
                        | Error error -> Error error
                        | Ok d ->
                            match eDecoder json with
                            | Error error -> Error error
                            | Ok e ->
                                match fDecoder json with
                                | Error error -> Error error
                                | Ok f ->
                                    match gDecoder json with
                                    | Error error -> Error error
                                    | Ok g -> Ok (combine a b c d e f g)
    let inline JsonDecode_map8
        ([<InlineIfLambda>] combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'combined)
        (aDecoder: JsonDecode_Decoder<'a>)
        (bDecoder: JsonDecode_Decoder<'b>)
        (cDecoder: JsonDecode_Decoder<'c>)
        (dDecoder: JsonDecode_Decoder<'d>)
        (eDecoder: JsonDecode_Decoder<'e>)
        (fDecoder: JsonDecode_Decoder<'f>)
        (gDecoder: JsonDecode_Decoder<'g>)
        (hDecoder: JsonDecode_Decoder<'h>)
        : JsonDecode_Decoder<'combined> =
        fun json ->
            match aDecoder json with
            | Error error -> Error error
            | Ok a ->
                match bDecoder json with
                | Error error -> Error error
                | Ok b ->
                    match cDecoder json with
                    | Error error -> Error error
                    | Ok c ->
                        match dDecoder json with
                        | Error error -> Error error
                        | Ok d ->
                            match eDecoder json with
                            | Error error -> Error error
                            | Ok e ->
                                match fDecoder json with
                                | Error error -> Error error
                                | Ok f ->
                                    match gDecoder json with
                                    | Error error -> Error error
                                    | Ok g ->
                                        match hDecoder json with
                                        | Error error -> Error error
                                        | Ok h -> Ok (combine a b c d e f g h)
    let JsonDecode_maybe (valueDecoder: JsonDecode_Decoder<'value>) : JsonDecode_Decoder<option<'value>> =
        fun json ->
            Ok
                (match valueDecoder json with
                 | Ok valueDecodeResult -> Some valueDecodeResult
                 | Error valueError -> None
                )
    let rec JsonDecode_oneOfWithErrorsReverse (errorsReverse: List<JsonDecode_Error>) (options: List<JsonDecode_Decoder<'value>>) : JsonDecode_Decoder<'value> =
        fun json ->
            match options with
            | [] -> Error (JsonDecode_OneOf (List.rev errorsReverse))
            | nextOptionToTry :: remainingOptions ->
                match nextOptionToTry json with
                | Ok value -> Ok value
                | Error error ->
                    JsonDecode_oneOfWithErrorsReverse
                        (error :: errorsReverse)
                        remainingOptions
                        json
    let JsonDecode_oneOf (options: List<JsonDecode_Decoder<'value>>) : JsonDecode_Decoder<'value> =
        JsonDecode_oneOfWithErrorsReverse [] options


    let JsonDecode_value : JsonDecode_Decoder<System.Text.Json.Nodes.JsonNode> =
        fun json -> Ok json
    let JsonDecode_string : JsonDecode_Decoder<StringRope> =
        fun json ->
            try Ok (StringRopeOne (json.AsValue().GetValue<string>())) with
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne "Expecting a STRING", json )))
    let JsonDecode_int : JsonDecode_Decoder<int64> =
        fun json ->
            try Ok (json.AsValue().GetValue<int64>()) with
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne "Expecting an INT", json )))
    let JsonDecode_float : JsonDecode_Decoder<float> =
        fun json ->
            try Ok (json.AsValue().GetValue<float>()) with
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne "Expecting a FLOAT", json )))
    let JsonDecode_bool : JsonDecode_Decoder<bool> =
        fun json ->
            try Ok (json.AsValue().GetValue<bool>()) with
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne "Expecting a BOOL", json )))
    let inline JsonDecode_null (value: 'value) : JsonDecode_Decoder<'value> =
        fun json ->
            match json.GetValueKind() with
            | System.Text.Json.JsonValueKind.Null ->
                Ok value
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne "Expecting NULL", json )))
    let JsonDecode_index (index: int64) (elementDecoder: JsonDecode_Decoder<'element>) : JsonDecode_Decoder<'element> =
        fun json ->
            if index <= 0 then
                Error
                    (JsonDecode_Failure
                        (struct(
                          StringRopeOne
                            ("Expecting an element at array index " + string index +
                                " (likely a logic error in decoder code)"
                            )
                        , json
                        ))
                    )
            else
            try
                let jsonArray: System.Text.Json.Nodes.JsonArray = json.AsArray()
                if index >= jsonArray.Count then
                    Error
                        (JsonDecode_Failure
                            (struct(
                              StringRopeOne
                                ("Expecting a LONGER array. Need index " + string index +
                                    " but only see " + string jsonArray.Count + " elements"
                                )
                            , json
                            ))
                        )
                else
                    match elementDecoder (jsonArray[int index]) with
                    | Ok elementDecoded ->
                        Ok elementDecoded
                    | Error error ->
                        Error (JsonDecode_Index (struct( index, error )))
            with
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne "Expecting an ARRAY", json )))
    let JsonDecode_list (elementDecoder: JsonDecode_Decoder<'element>) : JsonDecode_Decoder<List<'element>> =
        fun json ->
            try
                let jsonArray: System.Text.Json.Nodes.JsonArray = json.AsArray()
                let folded =
                    Seq.foldBack
                        (fun element (soFar: {| Index: int64; Result: Result<List<'element>, JsonDecode_Error> |}) ->
                            match soFar.Result with
                            | Error _ -> soFar
                            | Ok tail ->
                                {| Index = soFar.Index - 1L
                                ;  Result =
                                    match elementDecoder element with
                                    | Error error ->
                                        Error (JsonDecode_Index (struct( soFar.Index, error )))
                                    | Ok head ->
                                        Ok (head :: tail)
                                |}
                        )
                        jsonArray
                        {| Index = int64 jsonArray.Count; Result = Ok [] |}
                
                folded.Result
            with
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne "Expecting a LIST", json )))
    let JsonDecode_array (elementDecoder: JsonDecode_Decoder<'element>) : JsonDecode_Decoder<array<'element>> =
        // can be optimized
        JsonDecode_map Array.ofList (JsonDecode_list elementDecoder)
    
    let JsonDecode_field (fieldNameStringRope: StringRope) (valueDecoder: JsonDecode_Decoder<'value>) : JsonDecode_Decoder<'value> =
        let fieldNameString = StringRope.toString fieldNameStringRope
        fun json ->
            try
                let jsonObject: System.Text.Json.Nodes.JsonObject = json.AsObject()
                
                match jsonObject[fieldNameString] with
                | null ->
                    Error
                        (JsonDecode_Failure (struct( StringRopeOne ("Expecting an OBJECT with a field named '" + fieldNameString + "'"), json )))
                | fieldValueJson ->
                    match valueDecoder fieldValueJson with
                    | Ok fieldValue ->
                        Ok fieldValue
                    | Error error ->
                        Error (JsonDecode_Field (struct( fieldNameStringRope, error )))
            with
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne ("Expecting an OBJECT with a field named '" + fieldNameString + "'"), json )))
    let JsonDecode_at (fieldNames: List<StringRope>) (valueDecoder: JsonDecode_Decoder<'value>) : JsonDecode_Decoder<'value> =
        List.foldBack
            (fun (fieldName: StringRope) (decoderSoFar: JsonDecode_Decoder<'value>) ->
                JsonDecode_field fieldName decoderSoFar
            )
            fieldNames
            valueDecoder
    let JsonDecode_dict (valueDecoder: JsonDecode_Decoder<'value>) : JsonDecode_Decoder<Map<StringRope, 'value>> =
        fun json ->
            try
                let jsonObject: System.Text.Json.Nodes.JsonObject = json.AsObject()

                Seq.foldBack
                    (fun (field: System.Collections.Generic.KeyValuePair<string, System.Text.Json.Nodes.JsonNode>) (soFarOrError: Result<Map<StringRope, 'value>, JsonDecode_Error>) ->
                        match soFarOrError with
                        | Error _ -> soFarOrError
                        | Ok soFar ->
                            match valueDecoder field.Value with
                            | Error error ->
                                Error (JsonDecode_Field (struct( StringRopeOne field.Key, error )))
                            | Ok fieldValue ->
                                Ok (Map.add (StringRopeOne field.Key) fieldValue soFar)
                    )
                    jsonObject
                    (Ok Map.empty)
            with
            | _ ->
                Error (JsonDecode_Failure (struct( StringRopeOne "Expecting an OBJECT", json )))
    let JsonDecode_keyValuePairs (valueDecoder: JsonDecode_Decoder<'value>) : JsonDecode_Decoder<List<struct( StringRope * 'value )>> =
        // can be optimized
        JsonDecode_map Dict_toList (JsonDecode_dict valueDecoder)
    
    let JsonDecode_nullable (valueDecoder: JsonDecode_Decoder<'value>) : JsonDecode_Decoder<option<'value>> =
        fun json ->
            match JsonDecode_null None json with
            | Ok nullDecodeResult -> Ok nullDecodeResult
            | Error nullError ->
                match valueDecoder json with
                | Ok valueDecodeResult -> Ok (Some valueDecodeResult)
                | Error valueError ->   
                    Error (JsonDecode_OneOf [nullError; valueError])
    let inline JsonDecoder_oneOrMore
        ([<InlineIfLambda>] combineHeadTail: 'element -> List<'element> -> 'combined)
        (elementDecoder: JsonDecode_Decoder<'element>)
        : JsonDecode_Decoder<'combined> =
        JsonDecode_map2 combineHeadTail
            elementDecoder
            (JsonDecode_list elementDecoder)

    let inline indent (str: string) : string =
        String.concat "\\n    " (Array.toList (str.Split("\\n")))
    
    let rec JsonDecode_errorToStringHelp (error: JsonDecode_Error) (context: List<string>) : string =
        match error with
        | JsonDecode_Field(f, err) ->
            let isSimple =
                match String_uncons f with
                | None ->
                    false
                | Some(char, rest) ->
                    System.Char.IsLetter(char)
                        && String_all System.Char.IsLetter rest

            let fieldName =
                if isSimple then
                     "." + StringRope.toString f
                else
                    "['" + StringRope.toString f + "']"
            
            JsonDecode_errorToStringHelp err (fieldName :: context)

        | JsonDecode_Index(i, err) ->
            let indexName =
                "[" + string i + "]"
            
            JsonDecode_errorToStringHelp err (indexName :: context)

        | JsonDecode_OneOf(errors) ->
            match errors with
            | [] ->
                "Ran into a Json.Decode.oneOf with no possibilities" +
                    (match context with
                     | [] ->
                        "!"
                     | _ :: _ ->
                        " at json" + String.concat "" (List.rev context)
                    )

            | [ err ] ->
                JsonDecode_errorToStringHelp err context

            | _ :: _ :: _ ->
                let starter =
                    match context with
                    | [] ->
                        "Json.Decode.oneOf"
                    | _ :: _ ->
                        "The Json.Decode.oneOf at json" + String.concat "" (List.rev context)

                let introduction =
                    starter + " failed in the following " + string (List.length errors) + " ways:"
                
                String.concat "\\n\\n"
                    (introduction
                        :: List.mapi
                            (fun (i: int32) (error: JsonDecode_Error) ->
                                "\\n\\n(" + string (i + 1) + ") " + indent (JsonDecode_errorToStringHelp error [])
                            )
                            errors
                    )

        | JsonDecode_Failure(msg, json) ->
            let introduction =
                match context with
                | [] ->
                    "Problem with the given value:\\n\\n"
                | _ :: _ ->
                    "Problem with the value at json" + String.concat "" (List.rev context) + ":\\n\\n    "
            
            introduction + indent (StringRope.toString (JsonEncode_encode 4 json)) + "\\n\\n" + StringRope.toString msg

    let JsonDecode_errorToString (error: JsonDecode_Error) : string =
        JsonDecode_errorToStringHelp error []
    
    
    type Regex_Options =
        { CaseInsensitive: bool
        ; Multiline: bool
        }
    type Regex_Match =
        { Match: StringRope
        ; Index: int64
        ; Number: int64
        ; Submatches: List<option<StringRope>>
        }
    let Regex_fromString (string: StringRope) : option<System.Text.RegularExpressions.Regex> =
        try
            Some
                (System.Text.RegularExpressions.Regex(
                    StringRope.toString string,
                    System.Text.RegularExpressions.RegexOptions.ECMAScript
                ))
        with
        | _ ->
            None
    let Regex_fromStringWith (options: Regex_Options) (string: StringRope) : option<System.Text.RegularExpressions.Regex> =
        try
            Some
                (System.Text.RegularExpressions.Regex(
                    StringRope.toString string,
                    if options.Multiline then
                        if options.CaseInsensitive then
                            System.Text.RegularExpressions.RegexOptions.ECMAScript
                            ||| System.Text.RegularExpressions.RegexOptions.Multiline
                            ||| System.Text.RegularExpressions.RegexOptions.IgnoreCase
                        else
                            System.Text.RegularExpressions.RegexOptions.ECMAScript
                            ||| System.Text.RegularExpressions.RegexOptions.Multiline

                    else
                        // !options.Multiline
                        if options.CaseInsensitive then
                            System.Text.RegularExpressions.RegexOptions.ECMAScript
                            ||| System.Text.RegularExpressions.RegexOptions.Singleline
                            ||| System.Text.RegularExpressions.RegexOptions.IgnoreCase
                        else
                            System.Text.RegularExpressions.RegexOptions.ECMAScript
                            ||| System.Text.RegularExpressions.RegexOptions.Singleline
                ))
        with
        | _ ->
            None
    let Regex_never: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex("/.^/")
    let inline Regex_contains (regex: System.Text.RegularExpressions.Regex) (string: StringRope) : bool =
        regex.IsMatch(StringRope.toString string)
    let inline Regex_split (regex: System.Text.RegularExpressions.Regex) (string: StringRope) : List<StringRope> =
        // can be optimized
        Array.toList (Array.map StringRopeOne (regex.Split(StringRope.toString string)))
    let inline Regex_splitAtMost (maxSplitCount: int64) (regex: System.Text.RegularExpressions.Regex) (string: StringRope) : List<StringRope> =
        // can be optimized
        Array.toList
            (Array.map StringRopeOne
                (regex.Split(StringRope.toString string, int maxSplitCount))
            )

    let inline regexMatchToRegex_MatchAtIndex0Based (matchNumber0Based: int64) (regexMatch: System.Text.RegularExpressions.Match) : Regex_Match =
        { Match = StringRopeOne regexMatch.Value
        ; Index = regexMatch.Index
        ; Number = matchNumber0Based + 1L
        ; Submatches =
            Seq.toList
                (Seq.map
                    (fun (subMatch: System.Text.RegularExpressions.Group) ->
                        // TODO when does elm return None?
                        Some (StringRopeOne subMatch.Value)
                    )
                    regexMatch.Groups
                )
        }
    let inline Regex_find (regex: System.Text.RegularExpressions.Regex) (string: StringRope) : List<Regex_Match> =
        Seq.toList
            (Seq.mapi
                (fun index regexMatch ->
                    regexMatchToRegex_MatchAtIndex0Based index regexMatch
                )
                (regex.Matches(StringRope.toString string))
            )
    let inline Regex_findAtMost (maxMatchCount: int64) (regex: System.Text.RegularExpressions.Regex) (string: StringRope) : List<Regex_Match> =
        Seq.toList
            (Seq.mapi
                (fun index regexMatch ->
                    regexMatchToRegex_MatchAtIndex0Based index regexMatch
                )
                (regex.Matches(StringRope.toString string, int maxMatchCount))
            )
    let inline createRegexMatchNumber0BasedMap (regex: System.Text.RegularExpressions.Regex) (string: string) : Map<string, int64> =
        (Seq.fold
            (fun (soFar: {| Index: int64; Map: Map<string, int64> |}) (regexMatch: System.Text.RegularExpressions.Match) ->
                {| Index = soFar.Index + 1L
                ;  Map = Map.add regexMatch.Value soFar.Index soFar.Map
                |}
            )
            {| Index = 0L; Map = Map.empty |}
            (regex.Matches(string))
        ).Map
    let Regex_replace
        (regex: System.Text.RegularExpressions.Regex)
        (replacementForMatch: Regex_Match -> StringRope)
        (stringRope: StringRope)
        : StringRope =
        let string = StringRope.toString stringRope
        let matchNumbers0Based: Map<string, int64> =
            createRegexMatchNumber0BasedMap regex string

        StringRopeOne
            (regex.Replace(
                string,
                System.Text.RegularExpressions.MatchEvaluator(fun regexMatch ->
                    StringRope.toString
                        (replacementForMatch
                            (regexMatchToRegex_MatchAtIndex0Based
                                (Map.find regexMatch.Value matchNumbers0Based)
                                regexMatch
                            )
                        )
                )
            ))
    let Regex_replaceAtMost
        (maxMatchReplacementCount: int64)
        (regex: System.Text.RegularExpressions.Regex)
        (replacementForMatch: Regex_Match -> StringRope)
        (stringRope: StringRope)
        : StringRope =
        let string = StringRope.toString stringRope
        let matchNumbers0Based: Map<string, int64> =
            createRegexMatchNumber0BasedMap regex string

        StringRopeOne
            (regex.Replace(
                string,
                System.Text.RegularExpressions.MatchEvaluator(fun regexMatch ->
                    StringRope.toString
                        (replacementForMatch
                            (regexMatchToRegex_MatchAtIndex0Based
                                (Map.find regexMatch.Value matchNumbers0Based)
                                regexMatch
                            )
                        )
                ),
                int maxMatchReplacementCount
            ))

    
    let inline Debug_log (tag: StringRope) (value: 'value) : 'value =
        System.Diagnostics.Debug.Print(StringRope.toString tag + ": {0}", value)

        value
    let inline Debug_toString (value: 'value) : StringRope =
        StringRopeOne (value.ToString())
    let inline Debug_todo (message: string) : 'value =
        raise (new System.NotImplementedException(message))
    

    let ElmKernelParser_isSubString
        (smallStringRope: StringRope)
        (offsetOriginal: int64)
        (rowOriginal: int64)
        (colOriginal: int64)
        (bigStringRope: StringRope)
        : struct( int64 * int64 * int64 ) =
        let smallString = StringRope.toString smallStringRope
        let bigString = StringRope.toString bigStringRope
        let smallLength = String.length smallString
        let mutable row = int rowOriginal
        let mutable col = int colOriginal
        let mutable offset = int offsetOriginal
        let mutable isGood = int offset + smallLength <= String.length bigString
        let mutable i = 0
        while isGood && i < smallLength do
            let code = int (bigString[offset])
            isGood <- smallString[i] = bigString[offset]
            if code = 0x000A then // \\n
                i <- i + 1
                row <- row + 1
                col <- 1
            else
                col <- col + 1
                if (code &&& 0xF800) = 0xD800 then
                    isGood <- isGood && (smallString[i + 1] = bigString[offset + 1])
                    i <- i + 2
                    offset <- offset + 2
                else
                    i <- i + 1
        done

        (struct( (if isGood then offset else -1), row, col ))
    
    let ElmKernelParser_isSubChar (predicate: char -> bool) (offset: int64) (stringRope: StringRope) : int64 =
        let string = StringRope.toString stringRope
        if String.length string <= int offset then
            -1
        else if (int (string[int offset]) &&& 0xF800) = 0xD800 then
            (if predicate (char (string.Substring(int offset, 2))) then
                offset + 2L
             else
                -1L
            )
        else if predicate (string[int offset]) then
            (if (string[int offset] = '\\n') then
                -2L
             else
                offset + 1L
            )
        else
            -1

    let inline ElmKernelParser_isAsciiCode (code: int64) (offset: int64) (string: StringRope) : bool =
        int64 ((StringRope.toString string)[int offset]) = code

    let ElmKernelParser_chompBase10 (offsetOriginal: int64) (stringRope: StringRope) : int64 =
        let string = StringRope.toString stringRope
        let mutable offset = int offsetOriginal
        let mutable foundNonBase10 = false
        while (offset < String.length string) && not foundNonBase10 do
            foundNonBase10 <- not (System.Char.IsAsciiDigit(string[offset]))
            offset <- offset + 1
        done
        offset

    let ElmKernelParser_consumeBase
        (base_: int64)
        (offsetOriginal: int64)
        (stringRope: StringRope)
        : struct( int64 * int64 ) =
        let string = StringRope.toString stringRope
        let mutable offset = int offsetOriginal
        let mutable total = 0
        let mutable foundNonBase = false
        while (offset < String.length string) && not foundNonBase do
            let digit = int (string[offset]) - 0x30
            if (digit < 0 || base_ <= digit) then
                foundNonBase <- true
            else
                total <- int base_ * total + digit
                offset <- offset + 1
        done
        (struct( offset, total ))

    let ElmKernelParser_consumeBase16
        (offsetOriginal: int64)
        (stringRope: StringRope)
        : struct( int64 * int64 ) =
        let string = StringRope.toString stringRope
        let mutable offset = int offsetOriginal
        let mutable total = 0
        let mutable foundNonBase16 = false
        while (offset < String.length string) && not foundNonBase16 do
            let code = int (string[offset])
            if (0x30 <= code && code <= 0x39) then
                total <- 16 * total + code - 0x30
            else if (0x41 <= code && code <= 0x46) then
                total <- 16 * total + code - 55
            else if (0x61 <= code && code <= 0x66) then
                total <- 16 * total + code - 87
            else
                foundNonBase16 <- true
        done
        (struct( offset, total ))

    let ElmKernelParser_findSubString
        (smallStringRope: StringRope)
        (offsetOriginal: int64)
        (rowOriginal: int64)
        (colOriginal: int64)
        (bigStringRope: StringRope)
        : struct( int64 * int64 * int64 ) =
        let smallString = StringRope.toString smallStringRope
        let bigString = StringRope.toString bigStringRope
        let newOffset = bigString.IndexOf(smallString, int offsetOriginal)
        let target =
            if newOffset < 0 then 
                String.length bigString
            else
                newOffset + String.length smallString
        let mutable row = int rowOriginal
        let mutable col = int colOriginal
        let mutable offset = int offsetOriginal

        while (offset < target) do
            let code = int(bigString[offset])
            offset <- offset + 1
            if code = 0x000A then // \\n
                col <- 1
                row <- row + 1
            else
                col <- col + 1
                if (code &&& 0xF800) = 0xD800 then
                    offset <- offset + 1
        done

        (struct( newOffset, row, col ))

    let inline ElmKernelUrl_percentEncode (string: StringRope) : StringRope =
        StringRopeOne
            (System.Net.WebUtility.UrlEncode(StringRope.toString string))
    let inline ElmKernelUrl_percentDecode (string: StringRope) : option<StringRope> =
        match System.Net.WebUtility.UrlDecode(StringRope.toString string) with
        | null -> None
        | decodedString -> Some (StringRopeOne decodedString)


    [<Struct>]
    type Random_Seed =
        | Random_Seed of Random_Seed:
            // can be optimized by switching to int32
            (struct( int64 * int64 ))
    
    type Random_Generator<'a> =
        Random_Seed -> (struct( 'a * Random_Seed ))

    let Random_step (generator: Random_Generator<'a>) (seed: Random_Seed) =
        generator seed

    let Random_peel (Random_Seed(state, _): Random_Seed) =
        let word: int64 =
            (*) ((^^^) state (Bitwise_shiftRightZfBy ((+) (Bitwise_shiftRightZfBy 28L state) 4L) state)) 277803737L
         
        Bitwise_shiftRightZfBy 0L ((^^^) (Bitwise_shiftRightZfBy 22L word) word)

    let Random_next (Random_Seed(state0, incr): Random_Seed) =
        Random_Seed (struct( Bitwise_shiftRightZfBy 0L ((+) ((*) state0 1664525L) incr), incr ))

    let Random_minInt: int64 =
        -2147483648L

    let Random_maxInt: int64 =
        2147483647L

    let Random_map5 (func: 'a -> 'b -> 'c -> 'd -> 'e -> 'f) (genA: Random_Generator<'a>) (genB: Random_Generator<'b>) (genC: Random_Generator<'c>) (genD: Random_Generator<'d>) (genE: Random_Generator<'e>) =
        fun (seed0: Random_Seed) ->
            let ((struct( a, seed1 )):  (struct( 'a * Random_Seed ))) =
                genA seed0
                
            let ((struct( b, seed2 )): (struct( 'b * Random_Seed ))) =
                genB seed1
                
            let ((struct( c, seed3 )): (struct( 'c * Random_Seed ))) =
                genC seed2
                
            let ((struct( d, seed4 )): (struct( 'd * Random_Seed ))) =
                genD seed3
                
            let ((struct( e, seed5 )): (struct( 'e * Random_Seed ))) =
                genE seed4
                
            (struct( func a b c d e , seed5 ))

    let Random_map4 (func: 'a -> 'b -> 'c -> 'd -> 'e) (genA: Random_Generator<'a>) (genB: Random_Generator<'b>) (genC: Random_Generator<'c>) (genD: Random_Generator<'d>) =
        fun (seed0: Random_Seed) ->
            let ((struct( a, seed1 )): (struct( 'a * Random_Seed ))) =
                genA seed0
            
            let ((struct( b, seed2 )): (struct( 'b * Random_Seed ))) =
                genB seed1
            
            let ((struct( c, seed3 )): (struct( 'c * Random_Seed ))) =
                genC seed2
            
            let ((struct( d, seed4 )): (struct( 'd * Random_Seed ))) =
                genD seed3
            
            (struct( func a b c d , seed4 ))

    let Random_map3 (func: 'a -> 'b -> 'c -> 'd) (genA: Random_Generator<'a>) (genB: Random_Generator<'b>) (genC: Random_Generator<'c>) =
        fun (seed0: Random_Seed) ->
            let ((struct( a, seed1 )): (struct( 'a * Random_Seed ))) =
                genA seed0
                
            let ((struct( b, seed2 )): (struct( 'b * Random_Seed ))) =
                genB seed1
                
            let ((struct( c, seed3 )): (struct( 'c * Random_Seed ))) =
                genC seed2
                
            (struct( func a b c , seed3 ))

    let Random_map2 (func: 'a -> 'b -> 'c) (genA: Random_Generator<'a>) (genB: Random_Generator<'b>) =
        fun (seed0: Random_Seed) ->
            let ((struct( a, seed1 )): (struct( 'a * Random_Seed ))) =
                genA seed0
                
            let ((struct( b, seed2 )): (struct( 'b * Random_Seed ))) =
                genB seed1
                
            (struct( func a b , seed2 ))

    let Random_pair (genA: Random_Generator<'a>) (genB: Random_Generator<'b>) =
        Random_map2
            (fun (a: 'a) (b: 'b) ->
                (struct( a , b ))
            )
            genA
            genB

    let Random_map (func: 'a -> 'b) (genA: Random_Generator<'a>) =
        fun (seed0: Random_Seed) ->
            let ((struct( a, seed1 )): (struct( 'a * Random_Seed ))) =
                genA seed0
                
            (struct( func a , seed1 ))

    let rec Random_listHelp
        (revList: List<'a>)
        (n: int64)
        (gen: Random_Seed -> (struct( 'a * Random_Seed )))
        (seed: Random_Seed) =
        if (<) n 1L then
            (struct( revList , seed ))

        else
            let ((struct( value, newSeed )): (struct( 'a * Random_Seed ))) =
                gen seed
             
            Random_listHelp (List_cons value revList) ((-) n 1L) gen newSeed

    let Random_list (n: int64) (gen: Random_Generator<'a>) =
        fun (seed: Random_Seed) ->
            Random_listHelp [] n gen seed

    let Random_lazy (callback: unit -> Random_Generator<'a>) =
        fun (seed: Random_Seed) ->
            let (gen: Random_Generator<'a>) =
                callback ()
                
            gen seed

    let Random_int (a: int64) (b: int64) =
        fun (seed0: Random_Seed) ->
                let ((struct( lo, hi )):
                        (struct(
                          int64
                        * int64
                        ))
                    ) =
                    if (<) a b then
                        (struct( a , b ))

                    else
                        (struct( b , a ))
                 
                let range: int64 =
                    (+) ((-) hi lo) 1L
                 
                if Basics_eq ((&&&) ((-) range 1L) range) 0L then
                    (struct( (+) (Bitwise_shiftRightZfBy 0L ((&&&) ((-) range 1L) (Random_peel seed0))) lo , Random_next seed0 ))

                else
                    let threshold: int64 =
                        Bitwise_shiftRightZfBy 0L (Basics_remainderBy range (Bitwise_shiftRightZfBy 0L (Basics_inegate range)))
                     
                    let rec accountForBias (seed: Random_Seed) =
                        let x: int64 =
                            Random_peel seed
                         
                        let seedN: Random_Seed =
                            Random_next seed
                         
                        if (<) x threshold then
                            accountForBias seedN

                        else
                            (struct( (+) (Basics_remainderBy range x) lo, seedN ))
                    
                    accountForBias seed0

    let Random_initialSeed (x: int64) =
        let (Random_Seed(state1, incr): Random_Seed) =
            Random_next (Random_Seed (struct( 0L , 1013904223L )))
         
        let state2: int64 =
            Bitwise_shiftRightZfBy 0L ((+) state1 x)
         
        Random_next (Random_Seed (struct( state2 , incr )))

    let Random_independentSeed: Random_Generator<Random_Seed> =
        fun (seed0: Random_Seed) ->
            let makeIndependentSeed (state: int64) (b: int64) (c: int64) =
                Random_next (Random_Seed (struct( state, Bitwise_shiftRightZfBy 0L ((|||) 1L ((^^^) b c)) )))
                
            let gen: Random_Generator<int64> =
                Random_int 0L 4294967295L
                
            Random_step (Random_map3 makeIndependentSeed gen gen gen) seed0

    let rec Random_getByWeight
        ((struct( weight, value )): (struct( float * 'a )))
        (others: List<(struct( float * 'a ))>)
        (countdown: float) =
        match others with
        | [] ->
            value

        | second :: otherOthers ->
            if (<=) countdown (Basics_fabs weight) then
                value

            else
                Random_getByWeight second otherOthers ((-) countdown (Basics_fabs weight))

    let Random_float (a: float) (b: float) =
        fun (seed0: Random_Seed) ->
            let seed1: Random_Seed =
                Random_next seed0
                
            let range: float =
                Basics_fabs ((-) b a)
                
            let n1: int64 =
                Random_peel seed1
                
            let n0: int64 =
                Random_peel seed0
                
            let lo: float =
                (*) (float ((&&&) 134217727L n1)) 1.0
                
            let hi: float =
                (*) (float ((&&&) 67108863L n0)) 1.0
                
            let val_: float =
                (/) ((+) ((*) hi 134217728.0) lo) 9007199254740992.0
                
            let scaled: float =
                (+) ((*) val_ range) a
                
            (struct( scaled , Random_next seed1 ))

    let Random_weighted
        (first: (struct( float * 'a )))
        (others: List<(struct( float * 'a ))>) =
        let normalize ((struct( weight, _ )): (struct( float * 'ignored ))) =
            Basics_fabs weight
         
        let total: float =
            (+) (normalize first) (List.sum (List.map normalize others))
         
        Random_map (Random_getByWeight first others) (Random_float 0.0 total)

    let inline Random_constant (value: 'a) : Random_Generator<'a> =
        fun (seed: Random_Seed) ->
            (struct( value , seed ))

    let Random_andThen (callback: 'a -> Random_Generator<'b>) (genA: Random_Generator<'a>) =
        fun (seed: Random_Seed) ->
            let ((struct( result, newSeed )): (struct( 'a * Random_Seed ))) =
                genA seed
                
            let (genB: Random_Generator<'b>) =
                callback result
            
            genB newSeed

    let Random_addOne (value: 'a) =
        (struct( 1.0 , value ))

    let Random_uniform (value: 'a) (valueList: List<'a>) =
        Random_weighted (struct( 1.0, value )) (List.map Random_addOne valueList)


    [<Struct>]
    type Time_Posix =
        | Time_Posix of int64

    [<Struct>]
    type Time_Era =
        { Start: int64; Offset: int64 }
    
    [<Struct>]
    type Time_Zone =
        | Time_Zone of (struct( int64 * List<Time_Era> ))

    [<Struct>]
    type Time_Weekday =
        | Time_Mon
        | Time_Tue
        | Time_Wed
        | Time_Thu
        | Time_Fri
        | Time_Sat
        | Time_Sun

    [<Struct>]
    type Time_Month =
        | Time_Jan
        | Time_Feb
        | Time_Mar
        | Time_Apr
        | Time_May
        | Time_Jun
        | Time_Jul
        | Time_Aug
        | Time_Sep
        | Time_Oct
        | Time_Nov
        | Time_Dec
    
    [<Struct>]
    type Time_ZoneName =
        | Time_Name of Time_Name:
            StringRope
        | Time_Offset of Time_Offset:
            int64
    
    [<Struct>]
    type Time_Civil =
        { Year: int64; Month: int64; Day: int64 }
        
    let inline Time_posixToMillis ((Time_Posix millis): Time_Posix) : int64 =
        millis
    let inline Time_millisToPosix (millis: int64) : Time_Posix =
        Time_Posix millis

    let Time_utc: Time_Zone =
        Time_Zone (struct( 0L, [] ))
    let Time_customZone (n: int64) (eras: List<Time_Era>) : Time_Zone =
        Time_Zone (struct( n, eras ))
    
    let inline flooredDiv (numerator: int64) (denominator: int64) : int64 =
        int64 (floor (float numerator / float denominator))
    let rec Time_toAdjustedMinutesHelp (defaultOffset: int64) (posixMinutes: int64) (eras: List<Time_Era>) : int64 =
        match eras with
        | [] ->
            posixMinutes + defaultOffset

        | era :: olderEras ->
            if era.Start < posixMinutes then
                posixMinutes + era.Offset
            else
                Time_toAdjustedMinutesHelp defaultOffset posixMinutes olderEras
    let Time_toAdjustedMinutes ((Time_Zone (struct( defaultOffset, eras ))): Time_Zone) (time: Time_Posix) =
        Time_toAdjustedMinutesHelp defaultOffset (flooredDiv (Time_posixToMillis time) 60000) eras
    
    let Time_toCivil (minutes: int64) : Time_Civil =
        let rawDay    = flooredDiv minutes (60L * 24L) + 719468L
        let era       = (if rawDay >= 0 then rawDay else rawDay - 146096L) / 146097L
        let dayOfEra  = rawDay - era * 146097L // [0, 146096]
        let yearOfEra = (dayOfEra - dayOfEra / 1460L + dayOfEra / 36524L - dayOfEra / 146096L) / 365L // [0, 399]
        let year      = yearOfEra + era * 400L
        let dayOfYear = dayOfEra - (365L * yearOfEra + yearOfEra / 4L - yearOfEra / 100L) // [0, 365]
        let mp        = (5L * dayOfYear + 2L) / 153L // [0, 11]
        let month     = mp + (if mp < 10 then 3L else -9L) // [1, 12]
        
        { Year = year + (if month <= 2 then 1L else 0L)
        ; Month = month
        ; Day = dayOfYear - (153L * mp + 2L) / 5L + 1L // [1, 31]
        }

    let Time_toYear (zone: Time_Zone) (time: Time_Posix) : int64 =
        (Time_toCivil (Time_toAdjustedMinutes zone time)).Year
    let Time_toMonth (zone: Time_Zone) (time: Time_Posix) : Time_Month =
        match (Time_toCivil (Time_toAdjustedMinutes zone time)).Month with
        | 1L  -> Time_Jan
        | 2L  -> Time_Feb
        | 3L  -> Time_Mar
        | 4L  -> Time_Apr
        | 5L  -> Time_May
        | 6L  -> Time_Jun
        | 7L  -> Time_Jul
        | 8L  -> Time_Aug
        | 9L  -> Time_Sep
        | 10L -> Time_Oct
        | 11L -> Time_Nov
        | _  -> Time_Dec
    let Time_toDay (zone: Time_Zone) (time: Time_Posix) : int64 =
        (Time_toCivil (Time_toAdjustedMinutes zone time)).Day
    let Time_toWeekday (zone: Time_Zone) (time: Time_Posix) : Time_Weekday =
        match Basics_modBy 7 (flooredDiv (Time_toAdjustedMinutes zone time) (60L * 24L)) with
        | 0L -> Time_Thu
        | 1L -> Time_Fri
        | 2L -> Time_Sat
        | 3L -> Time_Sun
        | 4L -> Time_Mon
        | 5L -> Time_Tue
        | _ -> Time_Wed
    let Time_toHour zone time =
        Basics_modBy 24 (flooredDiv (Time_toAdjustedMinutes zone time) 60)
    let Time_toMinute (zone: Time_Zone) (time: Time_Posix) : int64 =
        Basics_modBy 60 (Time_toAdjustedMinutes zone time)
    let inline Time_toSecond (_: Time_Zone) (time: Time_Posix) : int64 =
        Basics_modBy 60 (flooredDiv (Time_posixToMillis time) 1000)
    let inline Time_toMillis (_: Time_Zone) (time: Time_Posix) : int64 =
        Basics_modBy 1000 (Time_posixToMillis time)
    

    type Bytes_Bytes =
        array<byte>
    
    [<Struct>]
    type Bytes_Endianness =
        | Bytes_LE
        | Bytes_BE

    type BytesEncode_Encoder =
        | BytesEncode_I8 of int64
        | BytesEncode_I16 of (struct( Bytes_Endianness * int64 ))
        | BytesEncode_I32 of (struct( Bytes_Endianness * int64 ))
        | BytesEncode_U8 of int64
        | BytesEncode_U16 of (struct( Bytes_Endianness * int64 ))
        | BytesEncode_U32 of (struct( Bytes_Endianness * int64 ))
        | BytesEncode_F32 of (struct( Bytes_Endianness * float ))
        | BytesEncode_F64 of (struct( Bytes_Endianness * float ))
        | BytesEncode_Seq of (struct( int * List<BytesEncode_Encoder> ))
        | BytesEncode_Utf8 of (struct( int * StringRope ))
        | BytesEncode_Bytes of Bytes_Bytes
    
    let Bytes_width (bytes: Bytes_Bytes) : int64 =
        Array.length bytes
    

    let BytesEncode_getStringWidth (string: StringRope): int64 =
        System.Text.Encoding.UTF8.GetByteCount(StringRope.toString string)

    let BytesEncode_signedInt8 (i8: int64) : BytesEncode_Encoder =
        BytesEncode_I8 i8

    let BytesEncode_signedInt16 (endianness : Bytes_Endianness) (i16: int64) : BytesEncode_Encoder =
        BytesEncode_I16 ( endianness, i16 )

    let BytesEncode_signedInt32 (endianness : Bytes_Endianness) (i32: int64) : BytesEncode_Encoder =
        BytesEncode_I32 ( endianness, i32 )

    let BytesEncode_unsignedInt8 (u8: int64) : BytesEncode_Encoder =
        BytesEncode_U8 u8

    let BytesEncode_unsignedInt16 (endianness : Bytes_Endianness) (u16: int64) : BytesEncode_Encoder =
        BytesEncode_U16 ( endianness, u16 )

    let BytesEncode_unsignedInt32 (endianness : Bytes_Endianness) (u32: int64) : BytesEncode_Encoder =
        BytesEncode_U32 ( endianness, u32 )

    let BytesEncode_float32 (endianness : Bytes_Endianness) (f32: float) : BytesEncode_Encoder =
        BytesEncode_F32 ( endianness, f32 )

    let BytesEncode_float64 (endianness : Bytes_Endianness) (f64: float) : BytesEncode_Encoder =
        BytesEncode_F64 ( endianness, f64 )

    let BytesEncode_bytes (bytes: Bytes_Bytes) : BytesEncode_Encoder =
        BytesEncode_Bytes bytes

    let BytesEncode_string (string: StringRope) : BytesEncode_Encoder =
        BytesEncode_Utf8
            ( System.Text.Encoding.UTF8.GetByteCount(StringRope.toString string)
            , string
            )
    
    let BytesEncode_EncoderByteCount (encoder: BytesEncode_Encoder) : int32 =
        match encoder with
        | BytesEncode_I8(_) -> 1
        | BytesEncode_I16(_) -> 2
        | BytesEncode_I32(_) -> 4
        | BytesEncode_U8(_) -> 1
        | BytesEncode_U16(_) -> 2
        | BytesEncode_U32(_) -> 4
        | BytesEncode_F32(_) -> 4
        | BytesEncode_F64(_) -> 8
        | BytesEncode_Seq( w, _ ) -> w
        | BytesEncode_Utf8( w, _ ) -> w
        | BytesEncode_Bytes bytes -> Array.length bytes

    let BytesEncode_sequence (encoders: List<BytesEncode_Encoder>) : BytesEncode_Encoder =
        BytesEncode_Seq
            ( Seq.sum (Seq.map BytesEncode_EncoderByteCount encoders)
            , encoders
            )
    
    let convertedBytesAdaptEndianness (endianness: Bytes_Endianness) (asLeBytes: Bytes_Bytes) : array<byte> =
        // can be optimized to use specialized endian operations
        // https://learn.microsoft.com/en-us/dotnet/api/system.buffers.binary.binaryprimitives?view=net-9.0
        if (endianness = Bytes_LE) <> System.BitConverter.IsLittleEndian then
            System.Array.Reverse(asLeBytes)
    
        asLeBytes

    let BytesEncode_encode (encoder: BytesEncode_Encoder) : Bytes_Bytes =
        let mutableBuffer = new System.IO.MemoryStream(BytesEncode_EncoderByteCount encoder)
        let mutable toEncodeNext = encoder
        let mutable mutableRemainingRightEncoders = System.Collections.Generic.Stack<BytesEncode_Encoder>()
        let mutable shouldKeepGoing = true

        while shouldKeepGoing do
            match toEncodeNext with
            | BytesEncode_I8(i8) ->
                mutableBuffer.WriteByte(byte (sbyte i8))
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_I16( endianness, i16 ) ->
                mutableBuffer.Write(convertedBytesAdaptEndianness endianness (System.BitConverter.GetBytes(int16 i16)))
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_I32( endianness, i32 ) ->
                mutableBuffer.Write(convertedBytesAdaptEndianness endianness (System.BitConverter.GetBytes(int32 i32)))
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_U8(u8) ->
                mutableBuffer.WriteByte(byte u8)
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_U16( endianness, u16 ) ->
                mutableBuffer.Write(convertedBytesAdaptEndianness endianness (System.BitConverter.GetBytes(uint16 u16)))
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_U32( endianness, u32 ) ->
                mutableBuffer.Write(convertedBytesAdaptEndianness endianness (System.BitConverter.GetBytes(uint32 u32)))
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_F32( endianness, f32 ) ->
                mutableBuffer.Write(convertedBytesAdaptEndianness endianness (System.BitConverter.GetBytes(float32 f32)))
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_F64( endianness, f64 ) ->
                mutableBuffer.Write(convertedBytesAdaptEndianness endianness (System.BitConverter.GetBytes(f64)))
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_Utf8( byteLength, stringRope ) ->
                mutableBuffer.Write(
                    new System.ReadOnlySpan<byte>(
                        System.Text.Encoding.UTF8.GetBytes(StringRope.toString stringRope)
                    )
                )
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_Bytes(byteArray) ->
                mutableBuffer.Write(new System.ReadOnlySpan<byte>(byteArray))
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_Seq( byteLength, encoders ) ->
                match encoders with
                | [] ->
                    if mutableRemainingRightEncoders.Count = 0 then
                        shouldKeepGoing <- false
                    else
                        toEncodeNext <- mutableRemainingRightEncoders.Pop()
                | nextSubEncoder :: subEncodersAfterNextSubEncoder ->
                    toEncodeNext <- nextSubEncoder
                    // can probably be optimized
                    Seq.iter
                        (fun subEncoderAfterNextSubEncoder ->
                            mutableRemainingRightEncoders.Push(subEncoderAfterNextSubEncoder)
                        )
                        (Seq.rev subEncodersAfterNextSubEncoder)
        done

        mutableBuffer.ToArray()
    

    type BytesDecode_Decoder<'value> =
        Bytes_Bytes -> int32 -> ValueOption<struct( int32 * 'value )>
    
    type BytesDecode_Step<'state, 'a> =
        | BytesDecode_Loop of 'state
        | BytesDecode_Done of 'a
    
    let BytesDecode_decode (decoder: BytesDecode_Decoder<'value>) (bytes: Bytes_Bytes) : option<'value> =
        match decoder bytes 0 with
        | ValueNone -> None
        | ValueSome(_, value) -> Some value
    
    let BytesDecode_succeed (value: 'value) : BytesDecode_Decoder<'value> =
        fun bytes index ->
            ValueSome( index, value )
    let BytesDecode_fail : BytesDecode_Decoder<'value> =
        fun bytes index ->
            ValueNone
    let BytesDecode_andThen (valueToFollowingDecoder: 'value -> BytesDecode_Decoder<'mappedValue>) (decoder: BytesDecode_Decoder<'value>) : BytesDecode_Decoder<'mappedValue> =
        fun bytes index ->
            match decoder bytes index with
            | ValueNone -> ValueNone
            | ValueSome( indexAfter, value ) ->
                valueToFollowingDecoder value bytes indexAfter
    let BytesDecode_map (valueChange: 'value -> 'mappedValue) (decoder: BytesDecode_Decoder<'value>) : BytesDecode_Decoder<'mappedValue> =
        fun bytes index ->
            match decoder bytes index with
            | ValueNone -> ValueNone
            | ValueSome( indexAfter, value ) ->
                ValueSome( indexAfter, valueChange value )
    
    let rec BytesDecode_loop
        (initialState: 'state)
        (step: 'state -> BytesDecode_Decoder<BytesDecode_Step<'state, 'a>>)
        : BytesDecode_Decoder<'a> =
        fun bytes index ->
            match step initialState bytes index with
            | ValueNone -> ValueNone
            | ValueSome(indexAfterStep, stepValue) ->            
                match stepValue with
                | BytesDecode_Loop(newState) ->
                    BytesDecode_loop newState step bytes indexAfterStep

                | BytesDecode_Done(result) ->
                    ValueSome( indexAfterStep, result )

    let BytesDecode_map2 (valuesCombine: 'a -> 'b -> 'combined) (aDecoder: BytesDecode_Decoder<'a>) (bDecoder: BytesDecode_Decoder<'b>) : BytesDecode_Decoder<'combined> =
        fun bytes index ->
            match aDecoder bytes index with
            | ValueNone -> ValueNone
            | ValueSome( indexAfterA, a ) ->
                match bDecoder bytes indexAfterA with
                | ValueNone -> ValueNone
                | ValueSome( indexAfterB, b ) ->
                    ValueSome( indexAfterB, valuesCombine a b )
    let BytesDecode_map3 (valuesCombine: 'a -> 'b -> 'c -> 'combined) (aDecoder: BytesDecode_Decoder<'a>) (bDecoder: BytesDecode_Decoder<'b>) (cDecoder: BytesDecode_Decoder<'c>) : BytesDecode_Decoder<'combined> =
        fun bytes index ->
            match aDecoder bytes index with
            | ValueNone -> ValueNone
            | ValueSome( indexAfterA, a ) ->
                match bDecoder bytes indexAfterA with
                | ValueNone -> ValueNone
                | ValueSome( indexAfterB, b ) ->
                    match cDecoder bytes indexAfterA with
                    | ValueNone -> ValueNone
                    | ValueSome( indexAfterC, c ) ->
                        ValueSome( indexAfterC, valuesCombine a b c )
    let BytesDecode_map4 (valuesCombine: 'a -> 'b -> 'c -> 'd -> 'combined) (aDecoder: BytesDecode_Decoder<'a>) (bDecoder: BytesDecode_Decoder<'b>) (cDecoder: BytesDecode_Decoder<'c>) (dDecoder: BytesDecode_Decoder<'d>) : BytesDecode_Decoder<'combined> =
        fun bytes index ->
            match aDecoder bytes index with
            | ValueNone -> ValueNone
            | ValueSome( indexAfterA, a ) ->
                match bDecoder bytes indexAfterA with
                | ValueNone -> ValueNone
                | ValueSome( indexAfterB, b ) ->
                    match cDecoder bytes indexAfterA with
                    | ValueNone -> ValueNone
                    | ValueSome( indexAfterC, c ) ->
                        match dDecoder bytes indexAfterA with
                        | ValueNone -> ValueNone
                        | ValueSome( indexAfterD, d ) ->
                            ValueSome( indexAfterD, valuesCombine a b c d )
    let BytesDecode_map5 (valuesCombine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined) (aDecoder: BytesDecode_Decoder<'a>) (bDecoder: BytesDecode_Decoder<'b>) (cDecoder: BytesDecode_Decoder<'c>) (dDecoder: BytesDecode_Decoder<'d>) (eDecoder: BytesDecode_Decoder<'e>) : BytesDecode_Decoder<'combined> =
        fun bytes index ->
            match aDecoder bytes index with
            | ValueNone -> ValueNone
            | ValueSome( indexAfterA, a ) ->
                match bDecoder bytes indexAfterA with
                | ValueNone -> ValueNone
                | ValueSome( indexAfterB, b ) ->
                    match cDecoder bytes indexAfterA with
                    | ValueNone -> ValueNone
                    | ValueSome( indexAfterC, c ) ->
                        match dDecoder bytes indexAfterA with
                        | ValueNone -> ValueNone
                        | ValueSome( indexAfterD, d ) ->
                            match eDecoder bytes indexAfterA with
                            | ValueNone -> ValueNone
                            | ValueSome( indexAfterE, e ) ->
                                ValueSome( indexAfterE, valuesCombine a b c d e )

    let BytesDecode_signedInt8 : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 1
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome ( indexAfter, int64 (sbyte (bytes[index])) )
    let BytesDecode_signedInt16 (endianness: Bytes_Endianness) : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 2
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome
                    ( indexAfter
                    , int64
                        (match endianness with
                         | Bytes_LE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadInt16LittleEndian(bytes[index..indexAfter])
                         | Bytes_BE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadInt16BigEndian(bytes[index..indexAfter])
                        )
                    )
    let BytesDecode_signedInt32 (endianness: Bytes_Endianness) : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 4
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome
                    ( indexAfter
                    , int64
                        (match endianness with
                         | Bytes_LE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadInt32LittleEndian(bytes[index..indexAfter])
                         | Bytes_BE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(bytes[index..indexAfter])
                        )
                    )
    let BytesDecode_unsignedInt8 : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 1
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome ( indexAfter, int64 (bytes[index]) )
    let BytesDecode_unsignedInt16 (endianness: Bytes_Endianness) : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 2
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome
                    ( indexAfter
                    , int64
                        (match endianness with
                         | Bytes_LE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadUInt16LittleEndian(bytes[index..indexAfter])
                         | Bytes_BE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadUInt16BigEndian(bytes[index..indexAfter])
                        )
                    )
    let BytesDecode_unsignedInt32 (endianness: Bytes_Endianness) : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 4
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome
                    ( indexAfter
                    , int64
                        (match endianness with
                         | Bytes_LE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadUInt32LittleEndian(bytes[index..indexAfter])
                         | Bytes_BE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(bytes[index..indexAfter])
                        )
                    )
    let BytesDecode_float32 (endianness: Bytes_Endianness) : BytesDecode_Decoder<float> =
        fun bytes index ->
            let indexAfter = index + 4
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome
                    ( indexAfter
                    , float
                        (match endianness with
                         | Bytes_LE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadSingleLittleEndian(bytes[index..indexAfter])
                         | Bytes_BE ->
                            System.Buffers.Binary.BinaryPrimitives.ReadSingleBigEndian(bytes[index..indexAfter])
                        )
                    )
    let BytesDecode_float64 (endianness: Bytes_Endianness) : BytesDecode_Decoder<float> =
        fun bytes index ->
            let indexAfter = index + 8
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome
                    ( indexAfter
                    , match endianness with
                      | Bytes_LE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadDoubleLittleEndian(bytes[index..indexAfter])
                      | Bytes_BE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadDoubleBigEndian(bytes[index..indexAfter])
                    )
    let BytesDecode_bytes (byteCountToRead: int64) : BytesDecode_Decoder<Bytes_Bytes> =
        fun bytes index ->
            let indexAfter = index + int byteCountToRead
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome
                    ( indexAfter
                    , bytes[index..indexAfter]
                    )
    let BytesDecode_string (byteCountToRead: int64) : BytesDecode_Decoder<StringRope> =
        fun bytes index ->
            let indexAfter = index + 8
            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome
                    ( indexAfter
                    , StringRopeOne
                        (System.Text.Encoding.UTF8.GetString(bytes, index, int byteCountToRead))
                    )


    [<Struct>]
    type PlatformCmd_PortOutgoing =
        { Name: string; Value: System.Text.Json.Nodes.JsonNode }
    [<Struct>]
    type PlatformCmd_CmdSingle<'event> =
        | PlatformCmd_PortOutgoing of PlatformCmd_PortOutgoing:
            PlatformCmd_PortOutgoing
    type PlatformCmd_Cmd<'event> =
        List<PlatformCmd_CmdSingle<'event>>
    
    let PlatformCmd_none: PlatformCmd_Cmd<'event> =
        []
    let PlatformCmd_batch (subCommands: List<PlatformCmd_Cmd<'event>>) : PlatformCmd_Cmd<'event> =
        List.concat subCommands
    let PlatformCmd_singleMap
        (eventChange: 'event -> 'mappedEvent)
        (commandSingle: PlatformCmd_CmdSingle<'event>)
        : PlatformCmd_CmdSingle<'mappedEvent> =
        match commandSingle with
        | PlatformCmd_PortOutgoing(portOutgoing) ->
            PlatformCmd_PortOutgoing portOutgoing
    let PlatformCmd_map
        (eventChange: 'event -> 'mappedEvent)
        (command: PlatformCmd_Cmd<'event>)
        : PlatformCmd_Cmd<'mappedEvent> =
        List.map
            (fun single -> PlatformCmd_singleMap eventChange single)
            command
    let PlatformCmd_portOutgoingWithName
        (name: StringRope)
        (value: System.Text.Json.Nodes.JsonNode)
        : PlatformCmd_Cmd<'event> =
        [ PlatformCmd_PortOutgoing
            { Name = StringRope.toString name; Value = value }
        ]
    [<Struct>]
    type PlatformSub_PortIncoming<'event> =
        { Name: string
        ; OnValue: System.Text.Json.Nodes.JsonNode -> 'event
        }
    [<Struct>]
    type PlatformSub_SubSingle<'event> =
        | PlatformSub_PortIncoming of PlatformSub_PortIncoming:
            PlatformSub_PortIncoming<'event>
    type PlatformSub_Sub<'event> =
        List<PlatformSub_SubSingle<'event>>
    
    let PlatformSub_none: PlatformSub_Sub<'event> =
        []
    let PlatformSub_batch (subSubscriptions: List<PlatformSub_Sub<'event>>) : PlatformSub_Sub<'event> =
        List.concat subSubscriptions
    let PlatformSub_singleMap
        (eventChange: 'event -> 'mappedEvent)
        (subscriptionSingle: PlatformSub_SubSingle<'event>)
        : PlatformSub_SubSingle<'mappedEvent> =
        match subscriptionSingle with
        | PlatformSub_PortIncoming(portIncoming) ->
            PlatformSub_PortIncoming
                { Name = portIncoming.Name
                ; OnValue =
                    fun value ->
                        eventChange (portIncoming.OnValue value)
                }
    let PlatformSub_map
        (eventChange: 'event -> 'mappedEvent)
        (subscription: PlatformSub_Sub<'event>)
        : PlatformSub_Sub<'mappedEvent> =
        List.map
            (fun single -> PlatformSub_singleMap eventChange single)
            subscription
    let PlatformSub_portIncomingWithName
        (name: StringRope)
        (onValue: System.Text.Json.Nodes.JsonNode -> 'event)
        : PlatformSub_Sub<'event> =
        [ PlatformSub_PortIncoming
            { Name = StringRope.toString name; OnValue = onValue }
        ]
    
    [<Struct>]
    type Platform_Program<'flags, 'state, 'event> =
        { Init : 'flags -> struct( 'state * PlatformCmd_Cmd<'event> )
        ; Update : 'event -> 'state -> struct( 'state * PlatformCmd_Cmd<'event> )
        ; Subscriptions : 'state -> PlatformSub_Sub<'event>
        }
    let Platform_worker
        (config: Platform_Program<'flags, 'state, 'event>)
        : Platform_Program<'flags, 'state, 'event> =
        config
"""


fastDictMapAndToList :
    (key -> value -> element)
    -> FastDict.Dict key value
    -> List element
fastDictMapAndToList keyValueToElement fastDict =
    fastDict
        |> FastDict.foldr
            (\key value soFar ->
                keyValueToElement key value
                    :: soFar
            )
            []


listMapAndCombineOk : (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOk elementToResult list =
    listMapAndCombineOkFrom [] elementToResult list


listMapAndCombineOkFrom : List ok -> (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOkFrom soFar elementToResult list =
    case list of
        [] ->
            Ok (soFar |> List.reverse)

        head :: tail ->
            case head |> elementToResult of
                Err headErr ->
                    Err headErr

                Ok headOk ->
                    listMapAndCombineOkFrom (headOk :: soFar)
                        elementToResult
                        tail


printLinebreakLinebreakIndented : Print.Print
printLinebreakLinebreakIndented =
    Print.linebreak
        |> Print.followedBy Print.linebreakIndented


printExactlyUnderscore : Print
printExactlyUnderscore =
    Print.exactly "_"


elmKernelParserTypes : FastDict.Dict Elm.Syntax.ModuleName.ModuleName ElmSyntaxTypeInfer.ModuleTypes
elmKernelParserTypes =
    FastDict.singleton
        [ "Elm", "Kernel", "Parser" ]
        { signatures =
            FastDict.fromList
                [ ( "isSubString"
                  , inferredTypeFunction
                        [ typeString, typeInt, typeInt, typeInt, typeString ]
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeTriple
                                { part0 = typeInt
                                , part1 = typeInt
                                , part2 = typeInt
                                }
                            )
                        )
                  )
                , ( "isSubChar"
                  , inferredTypeFunction
                        [ inferredTypeFunction [ typeChar ] typeBool, typeInt, typeString ]
                        typeInt
                  )
                , ( "isAsciiCode"
                  , inferredTypeFunction
                        [ typeInt, typeInt, typeString ]
                        typeBool
                  )
                , ( "chompBase10"
                  , inferredTypeFunction
                        [ typeInt, typeString ]
                        typeInt
                  )
                , ( "consumeBase"
                  , inferredTypeFunction [ typeInt, typeInt, typeString ]
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeTuple
                                { part0 = typeInt
                                , part1 = typeInt
                                }
                            )
                        )
                  )
                , ( "consumeBase16"
                  , inferredTypeFunction
                        [ typeInt, typeString ]
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeTuple
                                { part0 = typeInt
                                , part1 = typeInt
                                }
                            )
                        )
                  )
                , ( "findSubString"
                  , inferredTypeFunction
                        [ typeString, typeInt, typeInt, typeInt, typeString ]
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeTriple
                                { part0 = typeInt
                                , part1 = typeInt
                                , part2 = typeInt
                                }
                            )
                        )
                  )
                ]
        , typeAliases = FastDict.empty
        , choiceTypes = FastDict.empty
        }


elmKernelUrlTypes : FastDict.Dict Elm.Syntax.ModuleName.ModuleName ElmSyntaxTypeInfer.ModuleTypes
elmKernelUrlTypes =
    FastDict.singleton
        [ "Elm", "Kernel", "Url" ]
        { signatures =
            FastDict.fromList
                [ ( "percentEncode"
                  , inferredTypeFunction
                        [ typeString ]
                        typeString
                  )
                , ( "percentDecode"
                  , inferredTypeFunction
                        [ typeString ]
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Maybe" ]
                                , name = "Maybe"
                                , arguments = [ typeString ]
                                }
                            )
                        )
                  )
                , ( "findSubString"
                  , inferredTypeFunction
                        [ typeString, typeInt, typeInt, typeInt, typeString ]
                        (ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeTriple
                                { part0 = typeInt
                                , part1 = typeInt
                                , part2 = typeInt
                                }
                            )
                        )
                  )
                ]
        , typeAliases = FastDict.empty
        , choiceTypes = FastDict.empty
        }


inferredTypeFunction :
    List (ElmSyntaxTypeInfer.Type variable)
    -> ElmSyntaxTypeInfer.Type variable
    -> ElmSyntaxTypeInfer.Type variable
inferredTypeFunction inputs output =
    case inputs of
        [] ->
            output

        input :: remainingInputs ->
            ElmSyntaxTypeInfer.TypeNotVariable
                (ElmSyntaxTypeInfer.TypeFunction
                    { input = input
                    , output =
                        inferredTypeFunction remainingInputs output
                    }
                )


typeBool : ElmSyntaxTypeInfer.Type variable_
typeBool =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = [ "Basics" ]
            , name = "Bool"
            , arguments = []
            }
        )


typeInt : ElmSyntaxTypeInfer.Type variable_
typeInt =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = [ "Basics" ]
            , name = "Int"
            , arguments = []
            }
        )


typeString : ElmSyntaxTypeInfer.Type variable_
typeString =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = [ "String" ]
            , name = "String"
            , arguments = []
            }
        )


typeChar : ElmSyntaxTypeInfer.Type variable_
typeChar =
    ElmSyntaxTypeInfer.TypeNotVariable
        (ElmSyntaxTypeInfer.TypeConstruct
            { moduleOrigin = [ "Char" ]
            , name = "Char"
            , arguments = []
            }
        )


elmBytesTypes : FastDict.Dict Elm.Syntax.ModuleName.ModuleName ElmSyntaxTypeInfer.ModuleTypes
elmBytesTypes =
    FastDict.fromList
        [ ( [ "Bytes" ]
          , { signatures =
                FastDict.fromList
                    [ ( "getHostEndianness"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Task" ]
                                , name = "Task"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeVariable "x"
                                    , ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                      )
                    , ( "width"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Bytes"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Bytes"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    , ( "Endianness"
                      , { parameters = []
                        , variants =
                            FastDict.fromList [ ( "LE", [] ), ( "BE", [] ) ]
                        }
                      )
                    ]
            }
          )
        , ( [ "Bytes", "Decode" ]
          , { signatures =
                FastDict.fromList
                    [ ( "andThen"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "bytes"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes" ]
                                                        , name = "Bytes"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "decode"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes" ]
                                                        , name = "Bytes"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fail"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Bytes", "Decode" ]
                                , name = "Decoder"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeVariable "a" ]
                                }
                            )
                      )
                    , ( "float32"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "float64"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "loop"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeVariable "state"
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "state"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Bytes"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Bytes"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Step"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "state"
                                                                                    , ElmSyntaxTypeInfer.TypeVariable
                                                                                        "a"
                                                                                    ]
                                                                                }
                                                                            )
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "b"
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map2"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "result"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Bytes"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Bytes"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "result"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map3"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "result"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Bytes"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Bytes"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Bytes"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "result"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map4"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "result"
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Bytes"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Bytes"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Bytes"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Bytes"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "result"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map5"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "result"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Bytes"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Bytes"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Bytes"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Bytes"
                                                                                                            , "Decode"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Decoder"
                                                                                                        , arguments =
                                                                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Bytes"
                                                                                                            , "Decode"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Decoder"
                                                                                                        , arguments =
                                                                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "result"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "signedInt16"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "signedInt32"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "signedInt8"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Bytes", "Decode" ]
                                , name = "Decoder"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                      )
                    , ( "string"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "succeed"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input = ElmSyntaxTypeInfer.TypeVariable "a"
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "unsignedInt16"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "unsignedInt32"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "unsignedInt8"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Bytes", "Decode" ]
                                , name = "Decoder"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Decoder"
                      , { parameters = [ "a" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    , ( "Step"
                      , { parameters = [ "state", "a" ]
                        , variants =
                            FastDict.fromList
                                [ ( "Loop"
                                  , [ ElmSyntaxTypeInfer.TypeVariable "state" ]
                                  )
                                , ( "Done"
                                  , [ ElmSyntaxTypeInfer.TypeVariable "a" ]
                                  )
                                ]
                        }
                      )
                    ]
            }
          )
        , ( [ "Bytes", "Encode" ]
          , { signatures =
                FastDict.fromList
                    [ ( "bytes"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Bytes"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Encode" ]
                                            , name = "Encoder"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "encode"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Encode" ]
                                            , name = "Encoder"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Bytes"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "float32"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Encode"
                                                            ]
                                                        , name = "Encoder"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "float64"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Float"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Encode"
                                                            ]
                                                        , name = "Encoder"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "getStringWidth"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "sequence"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Encode"
                                                            ]
                                                        , name = "Encoder"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Encode" ]
                                            , name = "Encoder"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "signedInt16"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Encode"
                                                            ]
                                                        , name = "Encoder"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "signedInt32"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Encode"
                                                            ]
                                                        , name = "Encoder"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "signedInt8"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Encode" ]
                                            , name = "Encoder"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "string"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Encode" ]
                                            , name = "Encoder"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "unsignedInt16"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Encode"
                                                            ]
                                                        , name = "Encoder"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "unsignedInt32"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Bytes" ]
                                            , name = "Endianness"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Int"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Bytes"
                                                            , "Encode"
                                                            ]
                                                        , name = "Encoder"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "unsignedInt8"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Bytes", "Encode" ]
                                            , name = "Encoder"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Encoder"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    ]
            }
          )
        ]


elmJsonTypes : FastDict.Dict Elm.Syntax.ModuleName.ModuleName ElmSyntaxTypeInfer.ModuleTypes
elmJsonTypes =
    FastDict.fromList
        [ ( [ "Json", "Decode" ]
          , { signatures =
                FastDict.fromList
                    [ ( "andThen"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "array"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "at"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "bool"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Json", "Decode" ]
                                , name = "Decoder"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                      )
                    , ( "decodeString"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Error"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            , ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "decodeValue"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Value"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Result" ]
                                                        , name = "Result"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Error"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            , ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "dict"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Dict" ]
                                                        , name = "Dict"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            , ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "errorToString"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Error"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fail"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "field"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "float"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Json", "Decode" ]
                                , name = "Decoder"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                      )
                    , ( "index"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "int"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Json", "Decode" ]
                                , name = "Decoder"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                      )
                    , ( "keyValuePairs"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeTuple
                                                                    { part0 =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "String"
                                                                                    ]
                                                                                , name =
                                                                                    "String"
                                                                                , arguments =
                                                                                    []
                                                                                }
                                                                            )
                                                                    , part1 =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "a"
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "lazy"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    ElmSyntaxTypeInfer.TypeUnit
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "list"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "value"
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "value"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map2"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "value"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "value"
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map3"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "value"
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Json"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Json"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "value"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map4"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "value"
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Json"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Json"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Json"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "value"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map5"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "value"
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Json"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Json"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Json"
                                                                                                            , "Decode"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Decoder"
                                                                                                        , arguments =
                                                                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Json"
                                                                                                            , "Decode"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Decoder"
                                                                                                        , arguments =
                                                                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "value"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map6"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input =
                                                                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "f"
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "value"
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Json"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Json"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Json"
                                                                                                            , "Decode"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Decoder"
                                                                                                        , arguments =
                                                                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                    { moduleOrigin =
                                                                                                                        [ "Json"
                                                                                                                        , "Decode"
                                                                                                                        ]
                                                                                                                    , name =
                                                                                                                        "Decoder"
                                                                                                                    , arguments =
                                                                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                            "f"
                                                                                                                        ]
                                                                                                                    }
                                                                                                                )
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                    { moduleOrigin =
                                                                                                                        [ "Json"
                                                                                                                        , "Decode"
                                                                                                                        ]
                                                                                                                    , name =
                                                                                                                        "Decoder"
                                                                                                                    , arguments =
                                                                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                            "value"
                                                                                                                        ]
                                                                                                                    }
                                                                                                                )
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map7"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input =
                                                                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "f"
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input =
                                                                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                                                                            "g"
                                                                                                                    , output =
                                                                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                                                                            "value"
                                                                                                                    }
                                                                                                                )
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Json"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Json"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Json"
                                                                                                            , "Decode"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Decoder"
                                                                                                        , arguments =
                                                                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                    { moduleOrigin =
                                                                                                                        [ "Json"
                                                                                                                        , "Decode"
                                                                                                                        ]
                                                                                                                    , name =
                                                                                                                        "Decoder"
                                                                                                                    , arguments =
                                                                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                            "f"
                                                                                                                        ]
                                                                                                                    }
                                                                                                                )
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input =
                                                                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                                { moduleOrigin =
                                                                                                                                    [ "Json"
                                                                                                                                    , "Decode"
                                                                                                                                    ]
                                                                                                                                , name =
                                                                                                                                    "Decoder"
                                                                                                                                , arguments =
                                                                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                                        "g"
                                                                                                                                    ]
                                                                                                                                }
                                                                                                                            )
                                                                                                                    , output =
                                                                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                                { moduleOrigin =
                                                                                                                                    [ "Json"
                                                                                                                                    , "Decode"
                                                                                                                                    ]
                                                                                                                                , name =
                                                                                                                                    "Decoder"
                                                                                                                                , arguments =
                                                                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                                        "value"
                                                                                                                                    ]
                                                                                                                                }
                                                                                                                            )
                                                                                                                    }
                                                                                                                )
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "map8"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "b"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                            "c"
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                        "d"
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "e"
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input =
                                                                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "f"
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input =
                                                                                                                        ElmSyntaxTypeInfer.TypeVariable
                                                                                                                            "g"
                                                                                                                    , output =
                                                                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                                { input =
                                                                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                                                                        "h"
                                                                                                                                , output =
                                                                                                                                    ElmSyntaxTypeInfer.TypeVariable
                                                                                                                                        "value"
                                                                                                                                }
                                                                                                                            )
                                                                                                                    }
                                                                                                                )
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Decode"
                                                                        ]
                                                                    , name =
                                                                        "Decoder"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "b"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Json"
                                                                                    , "Decode"
                                                                                    ]
                                                                                , name =
                                                                                    "Decoder"
                                                                                , arguments =
                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                        "c"
                                                                                    ]
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                { input =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                            { moduleOrigin =
                                                                                                [ "Json"
                                                                                                , "Decode"
                                                                                                ]
                                                                                            , name =
                                                                                                "Decoder"
                                                                                            , arguments =
                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                    "d"
                                                                                                ]
                                                                                            }
                                                                                        )
                                                                                , output =
                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                        (ElmSyntaxTypeInfer.TypeFunction
                                                                                            { input =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                        { moduleOrigin =
                                                                                                            [ "Json"
                                                                                                            , "Decode"
                                                                                                            ]
                                                                                                        , name =
                                                                                                            "Decoder"
                                                                                                        , arguments =
                                                                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                "e"
                                                                                                            ]
                                                                                                        }
                                                                                                    )
                                                                                            , output =
                                                                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                                                                        { input =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                    { moduleOrigin =
                                                                                                                        [ "Json"
                                                                                                                        , "Decode"
                                                                                                                        ]
                                                                                                                    , name =
                                                                                                                        "Decoder"
                                                                                                                    , arguments =
                                                                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                            "f"
                                                                                                                        ]
                                                                                                                    }
                                                                                                                )
                                                                                                        , output =
                                                                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                    { input =
                                                                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                                { moduleOrigin =
                                                                                                                                    [ "Json"
                                                                                                                                    , "Decode"
                                                                                                                                    ]
                                                                                                                                , name =
                                                                                                                                    "Decoder"
                                                                                                                                , arguments =
                                                                                                                                    [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                                        "g"
                                                                                                                                    ]
                                                                                                                                }
                                                                                                                            )
                                                                                                                    , output =
                                                                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                            (ElmSyntaxTypeInfer.TypeFunction
                                                                                                                                { input =
                                                                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                                            { moduleOrigin =
                                                                                                                                                [ "Json"
                                                                                                                                                , "Decode"
                                                                                                                                                ]
                                                                                                                                            , name =
                                                                                                                                                "Decoder"
                                                                                                                                            , arguments =
                                                                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                                                    "h"
                                                                                                                                                ]
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                , output =
                                                                                                                                    ElmSyntaxTypeInfer.TypeNotVariable
                                                                                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                                                                                            { moduleOrigin =
                                                                                                                                                [ "Json"
                                                                                                                                                , "Decode"
                                                                                                                                                ]
                                                                                                                                            , name =
                                                                                                                                                "Decoder"
                                                                                                                                            , arguments =
                                                                                                                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                                                                                                                    "value"
                                                                                                                                                ]
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                }
                                                                                                                            )
                                                                                                                    }
                                                                                                                )
                                                                                                        }
                                                                                                    )
                                                                                            }
                                                                                        )
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "maybe"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "null"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input = ElmSyntaxTypeInfer.TypeVariable "a"
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "nullable"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "oneOf"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "oneOrMore"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "a"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "value"
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Decode"
                                                            ]
                                                        , name = "Decoder"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "value"
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "string"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Json", "Decode" ]
                                , name = "Decoder"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                      )
                    , ( "succeed"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input = ElmSyntaxTypeInfer.TypeVariable "a"
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Decoder"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "value"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Json", "Decode" ]
                                , name = "Decoder"
                                , arguments =
                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Decode" ]
                                            , name = "Value"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                      )
                    ]
            , typeAliases =
                FastDict.fromList
                    [ ( "Value"
                      , { parameters = []
                        , type_ =
                            ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeConstruct
                                    { moduleOrigin = [ "Json", "Encode" ]
                                    , name = "Value"
                                    , arguments = []
                                    }
                                )
                        , recordFieldOrder = Nothing
                        }
                      )
                    ]
            , choiceTypes =
                FastDict.fromList
                    [ ( "Decoder"
                      , { parameters = [ "a" ]
                        , variants = FastDict.fromList []
                        }
                      )
                    , ( "Error"
                      , { parameters = []
                        , variants =
                            FastDict.fromList
                                [ ( "Field"
                                  , [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                    , ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Json", "Decode" ]
                                            , name = "Error"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                  )
                                , ( "Index"
                                  , [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                    , ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Json", "Decode" ]
                                            , name = "Error"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                  )
                                , ( "OneOf"
                                  , [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json", "Decode" ]
                                                        , name = "Error"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                    ]
                                  )
                                , ( "Failure"
                                  , [ ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                    , ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Json", "Decode" ]
                                            , name = "Value"
                                            , arguments = []
                                            }
                                        )
                                    ]
                                  )
                                ]
                        }
                      )
                    ]
            }
          )
        , ( [ "Json", "Encode" ]
          , { signatures =
                FastDict.fromList
                    [ ( "array"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Encode"
                                                            ]
                                                        , name = "Value"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Array" ]
                                                        , name = "Array"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Encode"
                                                            ]
                                                        , name = "Value"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "bool"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Bool"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Encode" ]
                                            , name = "Value"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "dict"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "k"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeVariable
                                                                "v"
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Encode"
                                                                        ]
                                                                    , name =
                                                                        "Value"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Dict"
                                                                        ]
                                                                    , name =
                                                                        "Dict"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeVariable
                                                                            "k"
                                                                        , ElmSyntaxTypeInfer.TypeVariable
                                                                            "v"
                                                                        ]
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Encode"
                                                                        ]
                                                                    , name =
                                                                        "Value"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "encode"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Encode"
                                                            ]
                                                        , name = "Value"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "float"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Float"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Encode" ]
                                            , name = "Value"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "int"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Encode" ]
                                            , name = "Value"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "list"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Encode"
                                                            ]
                                                        , name = "Value"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Encode"
                                                            ]
                                                        , name = "Value"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "null"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Json", "Encode" ]
                                , name = "Value"
                                , arguments = []
                                }
                            )
                      )
                    , ( "object"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "List" ]
                                            , name = "List"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeTuple
                                                        { part0 =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , part1 =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Json"
                                                                        , "Encode"
                                                                        ]
                                                                    , name =
                                                                        "Value"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Encode" ]
                                            , name = "Value"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    , ( "set"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeVariable
                                                    "a"
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Encode"
                                                            ]
                                                        , name = "Value"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Set" ]
                                                        , name = "Set"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeVariable
                                                                "a"
                                                            ]
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Json"
                                                            , "Encode"
                                                            ]
                                                        , name = "Value"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "string"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin =
                                                [ "Json", "Encode" ]
                                            , name = "Value"
                                            , arguments = []
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases = FastDict.fromList []
            , choiceTypes =
                FastDict.fromList
                    [ ( "Value"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    ]
            }
          )
        ]


elmRegexTypes : FastDict.Dict Elm.Syntax.ModuleName.ModuleName ElmSyntaxTypeInfer.ModuleTypes
elmRegexTypes =
    FastDict.fromList
        [ ( [ "Regex" ]
          , { signatures =
                FastDict.fromList
                    [ ( "contains"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Regex" ]
                                            , name = "Regex"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Basics" ]
                                                        , name = "Bool"
                                                        , arguments = []
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "find"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Regex" ]
                                            , name = "Regex"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Regex"
                                                                        ]
                                                                    , name =
                                                                        "Match"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "findAtMost"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Regex" ]
                                                        , name = "Regex"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Regex"
                                                                                    ]
                                                                                , name =
                                                                                    "Match"
                                                                                , arguments =
                                                                                    []
                                                                                }
                                                                            )
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromString"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "String" ]
                                            , name = "String"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Maybe" ]
                                            , name = "Maybe"
                                            , arguments =
                                                [ ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Regex" ]
                                                        , name = "Regex"
                                                        , arguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                }
                            )
                      )
                    , ( "fromStringWith"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Regex" ]
                                            , name = "Options"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Maybe" ]
                                                        , name = "Maybe"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Regex"
                                                                        ]
                                                                    , name =
                                                                        "Regex"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "never"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeConstruct
                                { moduleOrigin = [ "Regex" ]
                                , name = "Regex"
                                , arguments = []
                                }
                            )
                      )
                    , ( "replace"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Regex" ]
                                            , name = "Regex"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "Regex"
                                                                        ]
                                                                    , name =
                                                                        "Match"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "replaceAtMost"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Regex" ]
                                                        , name = "Regex"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "Regex"
                                                                                    ]
                                                                                , name =
                                                                                    "Match"
                                                                                , arguments =
                                                                                    []
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "String"
                                                                                    ]
                                                                                , name =
                                                                                    "String"
                                                                                , arguments =
                                                                                    []
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeFunction
                                                                    { input =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "String"
                                                                                    ]
                                                                                , name =
                                                                                    "String"
                                                                                , arguments =
                                                                                    []
                                                                                }
                                                                            )
                                                                    , output =
                                                                        ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "String"
                                                                                    ]
                                                                                , name =
                                                                                    "String"
                                                                                , arguments =
                                                                                    []
                                                                                }
                                                                            )
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "split"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Regex" ]
                                            , name = "Regex"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "String" ]
                                                        , name = "String"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "List" ]
                                                        , name = "List"
                                                        , arguments =
                                                            [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    , ( "splitAtMost"
                      , ElmSyntaxTypeInfer.TypeNotVariable
                            (ElmSyntaxTypeInfer.TypeFunction
                                { input =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeConstruct
                                            { moduleOrigin = [ "Basics" ]
                                            , name = "Int"
                                            , arguments = []
                                            }
                                        )
                                , output =
                                    ElmSyntaxTypeInfer.TypeNotVariable
                                        (ElmSyntaxTypeInfer.TypeFunction
                                            { input =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeConstruct
                                                        { moduleOrigin =
                                                            [ "Regex" ]
                                                        , name = "Regex"
                                                        , arguments = []
                                                        }
                                                    )
                                            , output =
                                                ElmSyntaxTypeInfer.TypeNotVariable
                                                    (ElmSyntaxTypeInfer.TypeFunction
                                                        { input =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "String"
                                                                        ]
                                                                    , name =
                                                                        "String"
                                                                    , arguments =
                                                                        []
                                                                    }
                                                                )
                                                        , output =
                                                            ElmSyntaxTypeInfer.TypeNotVariable
                                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                                    { moduleOrigin =
                                                                        [ "List"
                                                                        ]
                                                                    , name =
                                                                        "List"
                                                                    , arguments =
                                                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                                { moduleOrigin =
                                                                                    [ "String"
                                                                                    ]
                                                                                , name =
                                                                                    "String"
                                                                                , arguments =
                                                                                    []
                                                                                }
                                                                            )
                                                                        ]
                                                                    }
                                                                )
                                                        }
                                                    )
                                            }
                                        )
                                }
                            )
                      )
                    ]
            , typeAliases =
                FastDict.fromList
                    [ ( "Match"
                      , { parameters = []
                        , type_ =
                            ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeRecord
                                    (FastDict.fromList
                                        [ ( "index"
                                          , ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                    { moduleOrigin =
                                                        [ "Basics" ]
                                                    , name = "Int"
                                                    , arguments = []
                                                    }
                                                )
                                          )
                                        , ( "match"
                                          , ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                    { moduleOrigin =
                                                        [ "String" ]
                                                    , name = "String"
                                                    , arguments = []
                                                    }
                                                )
                                          )
                                        , ( "number"
                                          , ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                    { moduleOrigin =
                                                        [ "Basics" ]
                                                    , name = "Int"
                                                    , arguments = []
                                                    }
                                                )
                                          )
                                        , ( "submatches"
                                          , ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                    { moduleOrigin =
                                                        [ "List" ]
                                                    , name = "List"
                                                    , arguments =
                                                        [ ElmSyntaxTypeInfer.TypeNotVariable
                                                            (ElmSyntaxTypeInfer.TypeConstruct
                                                                { moduleOrigin =
                                                                    [ "Maybe"
                                                                    ]
                                                                , name =
                                                                    "Maybe"
                                                                , arguments =
                                                                    [ ElmSyntaxTypeInfer.TypeNotVariable
                                                                        (ElmSyntaxTypeInfer.TypeConstruct
                                                                            { moduleOrigin =
                                                                                [ "String"
                                                                                ]
                                                                            , name =
                                                                                "String"
                                                                            , arguments =
                                                                                []
                                                                            }
                                                                        )
                                                                    ]
                                                                }
                                                            )
                                                        ]
                                                    }
                                                )
                                          )
                                        ]
                                    )
                                )
                        , recordFieldOrder =
                            Just [ "match", "index", "number", "submatches" ]
                        }
                      )
                    , ( "Options"
                      , { parameters = []
                        , type_ =
                            ElmSyntaxTypeInfer.TypeNotVariable
                                (ElmSyntaxTypeInfer.TypeRecord
                                    (FastDict.fromList
                                        [ ( "caseInsensitive"
                                          , ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                    { moduleOrigin =
                                                        [ "Basics" ]
                                                    , name = "Bool"
                                                    , arguments = []
                                                    }
                                                )
                                          )
                                        , ( "multiline"
                                          , ElmSyntaxTypeInfer.TypeNotVariable
                                                (ElmSyntaxTypeInfer.TypeConstruct
                                                    { moduleOrigin =
                                                        [ "Basics" ]
                                                    , name = "Bool"
                                                    , arguments = []
                                                    }
                                                )
                                          )
                                        ]
                                    )
                                )
                        , recordFieldOrder =
                            Just [ "caseInsensitive", "multiline" ]
                        }
                      )
                    ]
            , choiceTypes =
                FastDict.fromList
                    [ ( "Regex"
                      , { parameters = [], variants = FastDict.fromList [] }
                      )
                    ]
            }
          )
        ]
