module ElmSyntaxToFsharp exposing
    ( modules, fsharpDeclarationsToModuleString
    , FsharpLetDeclaration(..), FsharpExpression(..), FsharpPattern(..), FsharpDeclaration(..), FsharpType(..)
    )

{-| Transpiling [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
declarations to fsharp.

@docs modules, fsharpDeclarationsToModuleString
@docs FsharpLetDeclaration, FsharpExpression, FsharpPattern, FsharpDeclaration, FsharpType

If you need more fine-grained helpers,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import FastDict
import FastSet
import Print exposing (Print)
import Unicode


{-| The sub-set of F# declaration syntax used in generated code
-}
type FsharpDeclaration
    = FsharpDeclarationValueOrFunction
        { name : String
        , expression : FsharpExpression
        , type_ : Maybe FsharpType
        }
    | FsharpDeclarationTypeAlias
        { name : String
        , parameters : List String
        , type_ : FsharpType
        }
    | FsharpDeclarationChoiceType
        { name : String
        , parameters : List String
        , variants : FastDict.Dict String (Maybe FsharpType)
        }


{-| The sub-set of F# type syntax used in generated code
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
    | FsharpTypeRecord (FastDict.Dict String FsharpType)
    | FsharpTypeVariable String
    | FsharpTypeFunction
        { input : FsharpType
        , output : FsharpType
        }


{-| The sub-set of F# pattern syntax used in generated code
-}
type FsharpPattern
    = FsharpPatternIgnore
    | FsharpPatternFloat Float
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
    | FsharpPatternVariant
        { moduleOrigin : Maybe String
        , name : String
        , value : Maybe FsharpPattern
        }
    | FsharpPatternTuple
        { part0 : FsharpPattern
        , part1 : FsharpPattern
        , part2Up : List FsharpPattern
        }


{-| The sub-set of F# expression syntax used in generated code
-}
type FsharpExpression
    = FsharpExpressionUnit
    | FsharpExpressionFloat Float
    | FsharpExpressionChar Char
    | FsharpExpressionString String
    | FsharpExpressionReference
        { moduleOrigin : Maybe String
        , name : String
        }
    | FsharpExpressionRecordAccessFunction String
    | FsharpExpressionRecordAccess
        { record : FsharpExpression
        , field : String
        }
    | FsharpExpressionTuple
        { part0 : FsharpExpression
        , part1 : FsharpExpression
        , part2Up : List FsharpExpression
        }
    | FsharpExpressionIfThenElse
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
        , argument0 : FsharpExpression
        , argument1Up : List FsharpExpression
        }
    | FsharpExpressionLambda
        { parameter0 : FsharpPattern
        , parameter1Up : List FsharpPattern
        , result : FsharpExpression
        }
    | FsharpExpressionMatchWith
        { matched : FsharpExpression
        , case0 :
            { pattern : FsharpPattern
            , result : FsharpExpression
            }
        , case1Up :
            List
                { pattern : FsharpPattern
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
        , expression : FsharpExpression
        }
    | FsharpLetDeclarationValueOrFunction
        { name : String
        , expression : FsharpExpression
        , type_ : Maybe FsharpType
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
                                        }
                                    exposes =
                                        case syntaxImport.exposingList of
                                            Nothing ->
                                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                    FastSet.empty
                                                , variants = FastDict.empty
                                                }

                                            Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                                case moduleExposes |> FastDict.get importModuleName of
                                                    Nothing ->
                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                            FastSet.empty
                                                        , variants = FastDict.empty
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
                                                                                    }

                                                                                Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
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
                                                                                    }
                                                                        )
                                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                            FastSet.empty
                                                                        , variants = FastDict.empty
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
                        }
                    importedModuleMembers =
                        case moduleExposes |> FastDict.get syntaxImport.moduleName of
                            Nothing ->
                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                    FastSet.empty
                                , variants = FastDict.empty
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
                                }
                        )
                    )
                    soFar
            )
            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                FastDict.empty
            , variantLookup =
                FastDict.empty
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
    }


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
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
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "List" ]
      }
    , { moduleName = [ "Maybe" ]
      , alias = Nothing
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
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "String" ]
      }
    , { moduleName = [ "Char" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Char" ]
      }
    , { moduleName = [ "Tuple" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.empty
      }
    , { moduleName = [ "Debug" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.empty
      }
    , { moduleName = [ "Platform" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Program" ]
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , alias = Just "Cmd"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Cmd" ]
      }
    , { moduleName = [ "Platform", "Sub" ]
      , alias = Just "Sub"
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
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
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
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
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
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
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
    }


printFsharpDeclaration : FsharpDeclaration -> Print
printFsharpDeclaration fsharpDeclaration =
    case fsharpDeclaration of
        FsharpDeclarationValueOrFunction fsharpValueOrFunctionDeclaration ->
            printFsharpValueOrFunctionDeclaration fsharpValueOrFunctionDeclaration

        FsharpDeclarationTypeAlias fsharpTypeAliasDeclaration ->
            printFsharpTypeAliasDeclaration fsharpTypeAliasDeclaration

        FsharpDeclarationChoiceType fsharpChoiceTypeDeclaration ->
            printFsharpChoiceTypeDeclaration fsharpChoiceTypeDeclaration


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
            { name = syntaxChoiceType.name |> Elm.Syntax.Node.value
            , parameters =
                syntaxChoiceType.generics
                    |> List.map Elm.Syntax.Node.value
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

                                [ singleValue ] ->
                                    Just singleValue

                                value0 :: value1 :: value2Up ->
                                    Just
                                        (FsharpTypeTuple
                                            { part0 = value0
                                            , part1 = value1
                                            , part2Up = value2Up
                                            }
                                        )
                            )
                        )
                        (syntaxVariant.arguments
                            |> listMapAndCombineOk
                                (\value ->
                                    value |> type_ moduleOriginLookup
                                )
                        )
                )
        )


printFsharpChoiceTypeDeclaration :
    { name : String
    , parameters : List String
    , variants : FastDict.Dict String (Maybe FsharpType)
    }
    -> Print
printFsharpChoiceTypeDeclaration fsharpChoiceType =
    Print.exactly
        ("type "
            ++ fsharpChoiceType.name
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
                                    printFsharpVariant
                                        { name = name
                                        , maybeValue = maybeValue
                                        }
                                )
                                Print.linebreakIndented
                        )
                )
            )


printFsharpVariant : { name : String, maybeValue : Maybe FsharpType } -> Print
printFsharpVariant fsharpVariant =
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
                    Print.exactly " of"
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
            { name = syntaxTypeAlias.name |> Elm.Syntax.Node.value
            , parameters = syntaxTypeAlias.generics |> List.map Elm.Syntax.Node.value
            , type_ = aliasedType
            }
        )
        (syntaxTypeAlias.typeAnnotation |> type_ moduleOriginLookup)


printFsharpTypeAliasDeclaration :
    { name : String
    , parameters : List String
    , type_ : FsharpType
    }
    -> Print
printFsharpTypeAliasDeclaration fsharpTypeAliasDeclaration =
    Print.exactly
        ("type "
            ++ fsharpTypeAliasDeclaration.name
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


type_ :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String FsharpType
type_ moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxType) =
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
                                            |> referenceToCoreFsharp
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
                                (\argument -> argument |> type_ moduleOriginLookup)
                        )

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Ok fsharpTypeUnit

                [ inParens ] ->
                    type_ moduleOriginLookup inParens

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            FsharpTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            FsharpTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)
                        (tuplePart2 |> type_ moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.TypeAnnotation.Record recordFields ->
            Result.map (\fields -> FsharpTypeRecord (FastDict.fromList fields))
                (recordFields
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, valueNode )) ->
                            Result.map
                                (\value ->
                                    ( name |> stringFirstCharToUpper, value )
                                )
                                (valueNode |> type_ moduleOriginLookup)
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
                (inputNode |> type_ moduleOriginLookup)
                (outputNode |> type_ moduleOriginLookup)

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            Err "extensible record types are not supported"


fsharpTypeExpandFunctionOutput : FsharpType -> List FsharpType
fsharpTypeExpandFunctionOutput fsharpType =
    fsharpTypeExpandFunctionOutputInto [] fsharpType
        |> List.reverse


fsharpTypeExpandFunctionOutputInto : List FsharpType -> FsharpType -> List FsharpType
fsharpTypeExpandFunctionOutputInto soFar fsharpType =
    case fsharpType of
        FsharpTypeFunction function ->
            fsharpTypeExpandFunctionOutputInto
                (function.input :: soFar)
                function.output

        FsharpTypeConstruct construct ->
            FsharpTypeConstruct construct :: soFar

        FsharpTypeTuple parts ->
            FsharpTypeTuple parts :: soFar

        FsharpTypeRecord record ->
            FsharpTypeRecord record :: soFar

        FsharpTypeVariable variable ->
            FsharpTypeVariable variable :: soFar


fsharpTypeUnit : FsharpType
fsharpTypeUnit =
    FsharpTypeConstruct
        { moduleOrigin = Nothing
        , name = "unit"
        , arguments = []
        }


printFsharpTypeNotParenthesized : FsharpType -> Print
printFsharpTypeNotParenthesized fsharpType =
    -- IGNORE TCO
    case fsharpType of
        FsharpTypeVariable variable ->
            Print.exactly ("'" ++ variable)

        FsharpTypeConstruct typeConstruct ->
            case typeConstruct.arguments of
                [] ->
                    Print.exactly
                        (fsharpReferenceToString
                            { moduleOrigin = typeConstruct.moduleOrigin
                            , name = typeConstruct.name
                            }
                        )

                argument0 :: argument1Up ->
                    let
                        argumentPrints : List Print
                        argumentPrints =
                            (argument0 :: argument1Up)
                                |> List.map printFsharpTypeNotParenthesized

                        fullLineSpread : Print.LineSpread
                        fullLineSpread =
                            argumentPrints
                                |> Print.lineSpreadListMapAndCombine
                                    Print.lineSpread
                    in
                    Print.exactly
                        (fsharpReferenceToString
                            { moduleOrigin = typeConstruct.moduleOrigin
                            , name = typeConstruct.name
                            }
                        )
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.emptyOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        (Print.exactly "<")
                                    |> Print.followedBy
                                        (case fullLineSpread of
                                            Print.MultipleLines ->
                                                Print.exactly " "

                                            Print.SingleLine ->
                                                Print.empty
                                        )
                                    |> Print.followedBy
                                        (argumentPrints
                                            |> Print.listMapAndIntersperseAndFlatten
                                                (\argumentPrint ->
                                                    Print.withIndentIncreasedBy 2
                                                        argumentPrint
                                                )
                                                (Print.emptyOrLinebreakIndented fullLineSpread
                                                    |> Print.followedBy
                                                        (Print.exactly ", ")
                                                )
                                        )
                                    |> Print.followedBy
                                        (Print.emptyOrLinebreakIndented fullLineSpread)
                                    |> Print.followedBy
                                        (Print.exactly ">")
                                )
                            )

        FsharpTypeTuple parts ->
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
            Print.exactly "( "
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
                |> Print.followedBy (Print.exactly ")")

        FsharpTypeRecord typeRecord ->
            printFsharpTypeRecord typeRecord

        FsharpTypeFunction typeFunction ->
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
                                Print.withIndentIncreasedBy 3
                                    outputPartPrint
                            )
                   )
                |> Print.listIntersperseAndFlatten
                    (Print.spaceOrLinebreakIndented fullLineSpread
                        |> Print.followedBy
                            (Print.exactly "-> ")
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

        FsharpTypeRecord _ ->
            False

        FsharpTypeFunction _ ->
            True


printFsharpTypeParenthesizedIfSpaceSeparated : FsharpType -> Print
printFsharpTypeParenthesizedIfSpaceSeparated fsharpType =
    if fsharpType |> typeIsSpaceSeparated then
        printParenthesized
            (fsharpType |> printFsharpTypeNotParenthesized)

    else
        fsharpType |> printFsharpTypeNotParenthesized


printFsharpTypeRecord : FastDict.Dict String FsharpType -> Print
printFsharpTypeRecord syntaxRecordFields =
    if syntaxRecordFields |> FastDict.isEmpty then
        Print.exactly "{||}"

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
                            Print.withIndentIncreasedBy 3
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
                                (Print.exactly ";  ")
                        )
        in
        printExactlyCurlyOpeningVerticalBarSpace
            |> Print.followedBy fieldsPrint
            |> Print.followedBy
                (Print.spaceOrLinebreakIndented
                    (fieldsPrint |> Print.lineSpread)
                )
            |> Print.followedBy printExactlyVerticalBarCurlyClosing


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


stringLiteral : String -> Print
stringLiteral stringContent =
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
    Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


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
                "\\U" ++ characterHex otherCharacter

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
    String.toUpper
        (intToHexString (Char.toCode character)
            |> String.padLeft 8 '0'
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
                "\\U" ++ characterHex otherCharacter

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
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { pattern : FsharpPattern
            , introducedVariables : FastSet.Set String
            , fieldVariablesToIntroduce :
                List
                    { recordVariable : String
                    , field : String
                    }
            }
pattern moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { pattern = FsharpPatternIgnore
                , introducedVariables = FastSet.empty
                , fieldVariablesToIntroduce = []
                }

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { pattern = FsharpPatternIgnore
                , introducedVariables = FastSet.empty
                , fieldVariablesToIntroduce = []
                }

        Elm.Syntax.Pattern.CharPattern charValue ->
            Ok
                { pattern = FsharpPatternChar charValue
                , introducedVariables = FastSet.empty
                , fieldVariablesToIntroduce = []
                }

        Elm.Syntax.Pattern.StringPattern stringValue ->
            Ok
                { pattern = FsharpPatternString stringValue
                , introducedVariables = FastSet.empty
                , fieldVariablesToIntroduce = []
                }

        Elm.Syntax.Pattern.IntPattern intValue ->
            Ok
                { pattern = FsharpPatternFloat (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                , fieldVariablesToIntroduce = []
                }

        Elm.Syntax.Pattern.HexPattern intValue ->
            Ok
                { pattern = FsharpPatternFloat (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                , fieldVariablesToIntroduce = []
                }

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "float pattern is invalid syntax"

        Elm.Syntax.Pattern.VarPattern variableName ->
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
                , fieldVariablesToIntroduce = []
                }

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            pattern moduleOriginLookup inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [] ->
                    -- should be covered by UnitPattern
                    Ok
                        { pattern = FsharpPatternIgnore
                        , introducedVariables = FastSet.empty
                        , fieldVariablesToIntroduce = []
                        }

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    pattern moduleOriginLookup inParens

                [ part0Node, part1Node ] ->
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
                                    part0.introducedVariables
                            , fieldVariablesToIntroduce =
                                part0.fieldVariablesToIntroduce
                                    ++ part1.fieldVariablesToIntroduce
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)

                [ part0Node, part1Node, part2Node ] ->
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
                                        part0.introducedVariables
                                        part1.introducedVariables
                                    )
                            , fieldVariablesToIntroduce =
                                part0.fieldVariablesToIntroduce
                                    ++ part1.fieldVariablesToIntroduce
                                    ++ part2.fieldVariablesToIntroduce
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)
                        (part2Node |> pattern moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    Err "too many tuple parts"

        Elm.Syntax.Pattern.RecordPattern fields ->
            let
                fieldNames : List String
                fieldNames =
                    fields
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ lowercaseName) ->
                                lowercaseName |> stringFirstCharToUpper
                            )

                recordVariable : String
                recordVariable =
                    "record"
                        ++ (fieldNames |> String.concat)
                        |> variableNameDisambiguateFromFsharpKeywords
            in
            Ok
                { pattern = FsharpPatternVariable recordVariable
                , introducedVariables =
                    -- the record variable won't be used by user created code
                    FastSet.empty
                , fieldVariablesToIntroduce =
                    fieldNames
                        |> List.map
                            (\fieldName ->
                                { recordVariable = recordVariable
                                , field = fieldName
                                }
                            )
                }

        Elm.Syntax.Pattern.UnConsPattern headNode tailNode ->
            Result.map2
                (\head tail ->
                    { pattern =
                        FsharpPatternListCons
                            { head = head.pattern
                            , tail = tail.pattern
                            }
                    , introducedVariables =
                        FastSet.union
                            head.introducedVariables
                            tail.introducedVariables
                    , fieldVariablesToIntroduce =
                        head.fieldVariablesToIntroduce
                            ++ tail.fieldVariablesToIntroduce
                    }
                )
                (headNode |> pattern moduleOriginLookup)
                (tailNode |> pattern moduleOriginLookup)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Result.map
                (\elements ->
                    { pattern =
                        FsharpPatternListExact (elements |> List.map .pattern)
                    , introducedVariables =
                        elements
                            |> listMapToFastSetsAndUnify .introducedVariables
                    , fieldVariablesToIntroduce =
                        elements
                            |> List.concatMap .fieldVariablesToIntroduce
                    }
                )
                (elementPatterns
                    |> listMapAndCombineOk
                        (\element -> element |> pattern moduleOriginLookup)
                )

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            Result.map2
                (\values reference ->
                    { pattern =
                        FsharpPatternVariant
                            { value =
                                case values of
                                    [] ->
                                        Nothing

                                    [ onlyValue ] ->
                                        Just onlyValue.pattern

                                    value0 :: value1 :: value2Up ->
                                        Just
                                            (FsharpPatternTuple
                                                { part0 = value0.pattern
                                                , part1 = value1.pattern
                                                , part2Up =
                                                    value2Up
                                                        |> List.map .pattern
                                                }
                                            )
                            , moduleOrigin = reference.moduleOrigin
                            , name = reference.name
                            }
                    , introducedVariables =
                        values
                            |> listMapToFastSetsAndUnify .introducedVariables
                    , fieldVariablesToIntroduce =
                        values
                            |> List.concatMap .fieldVariablesToIntroduce
                    }
                )
                (argumentPatterns
                    |> listMapAndCombineOk
                        (\argument -> argument |> pattern moduleOriginLookup)
                )
                (case moduleOriginLookup.variantLookup |> FastDict.get ( syntaxQualifiedNameRef.moduleName, syntaxQualifiedNameRef.name ) of
                    Nothing ->
                        Err
                            ("could not find origin choice type for the variant "
                                ++ qualifiedToString
                                    { qualification = syntaxQualifiedNameRef.moduleName
                                    , name = syntaxQualifiedNameRef.name
                                    }
                            )

                    Just variantInfo ->
                        Ok
                            (case { moduleOrigin = variantInfo.moduleOrigin, name = syntaxQualifiedNameRef.name } |> referenceToCoreFsharp of
                                Just fsharpReference ->
                                    fsharpReference

                                Nothing ->
                                    { moduleOrigin = Nothing
                                    , name =
                                        referenceToFsharpName
                                            { moduleOrigin = variantInfo.moduleOrigin
                                            , name = syntaxQualifiedNameRef.name
                                            }
                                    }
                            )
                )

        Elm.Syntax.Pattern.AsPattern aliasedPatternNode (Elm.Syntax.Node.Node _ variable) ->
            Result.map
                (\aliasedPattern ->
                    let
                        variableDisambiguated : String
                        variableDisambiguated =
                            variable |> variableNameDisambiguateFromFsharpKeywords
                    in
                    { pattern =
                        FsharpPatternAs
                            { pattern = aliasedPattern.pattern
                            , variable = variableDisambiguated
                            }
                    , introducedVariables =
                        aliasedPattern.introducedVariables
                            |> FastSet.insert variableDisambiguated
                    , fieldVariablesToIntroduce =
                        aliasedPattern.fieldVariablesToIntroduce
                    }
                )
                (aliasedPatternNode |> pattern moduleOriginLookup)


referenceToCoreFsharp :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
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
                    Just { moduleOrigin = Nothing, name = "basicsAlways" }

                "compare" ->
                    Just { moduleOrigin = Nothing, name = "basics_compare" }

                "max" ->
                    Just { moduleOrigin = Nothing, name = "max" }

                "min" ->
                    Just { moduleOrigin = Nothing, name = "min" }

                "Order" ->
                    Just { moduleOrigin = Nothing, name = "Basics_Order" }

                "LT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_LT" }

                "EQ" ->
                    Just { moduleOrigin = Nothing, name = "Basics_EQ" }

                "GT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_GT" }

                "Bool" ->
                    Just { moduleOrigin = Nothing, name = "bool" }

                "True" ->
                    Just { moduleOrigin = Nothing, name = "true" }

                "False" ->
                    Just { moduleOrigin = Nothing, name = "false" }

                "not" ->
                    Just { moduleOrigin = Nothing, name = "not" }

                "xor" ->
                    Just { moduleOrigin = Nothing, name = "basics_neq" }

                "Int" ->
                    Just { moduleOrigin = Nothing, name = "float" }

                "Float" ->
                    Just { moduleOrigin = Nothing, name = "float" }

                "e" ->
                    Just { moduleOrigin = Just "System.Double", name = "E" }

                "pi" ->
                    Just { moduleOrigin = Just "System.Double", name = "Pi" }

                "ceiling" ->
                    Just { moduleOrigin = Nothing, name = "ceil" }

                "floor" ->
                    Just { moduleOrigin = Nothing, name = "floor" }

                "round" ->
                    Just { moduleOrigin = Nothing, name = "round" }

                "truncate" ->
                    Just { moduleOrigin = Nothing, name = "truncate" }

                "negate" ->
                    Just { moduleOrigin = Nothing, name = "basics_negate" }

                "abs" ->
                    Just { moduleOrigin = Just "System.Double", name = "Abs" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "id" }

                "isNaN" ->
                    Just { moduleOrigin = Just "System.Double", name = "IsNaN" }

                "isInfinite" ->
                    Just { moduleOrigin = Just "System.Double", name = "IsInfinity" }

                "remainderBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_remainderBy" }

                "modBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_modBy" }

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

                "sqrt" ->
                    Just { moduleOrigin = Just "System.Double", name = "Sqrt" }

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "String" ->
                    Just { moduleOrigin = Nothing, name = "string" }

                "isEmpty" ->
                    Just { moduleOrigin = Nothing, name = "string_isEmpty" }

                "length" ->
                    Just { moduleOrigin = Just "String", name = "length" }

                "append" ->
                    Just { moduleOrigin = Nothing, name = "string_concat" }

                "trim" ->
                    Just { moduleOrigin = Nothing, name = "string_trim" }

                "trimLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_trimLeft" }

                "trimRight" ->
                    Just { moduleOrigin = Nothing, name = "string_trimRight" }

                "left" ->
                    Just { moduleOrigin = Nothing, name = "string_left" }

                "right" ->
                    Just { moduleOrigin = Nothing, name = "string_right" }

                "dropLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_dropLeft" }

                "dropRight" ->
                    Just { moduleOrigin = Nothing, name = "string_dropRight" }

                "padLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_padLeft" }

                "padRight" ->
                    Just { moduleOrigin = Nothing, name = "string_padRight" }

                "toList" ->
                    Just { moduleOrigin = Nothing, name = "string_toList" }

                "join" ->
                    Just { moduleOrigin = Just "String", name = "concat" }

                "filter" ->
                    Just { moduleOrigin = Just "String", name = "filter" }

                "any" ->
                    Just { moduleOrigin = Just "String", name = "exists" }

                "all" ->
                    Just { moduleOrigin = Just "String", name = "forall" }

                "map" ->
                    Just { moduleOrigin = Just "String", name = "map" }

                "repeat" ->
                    Just { moduleOrigin = Just "String", name = "replicate" }

                "split" ->
                    Just { moduleOrigin = Nothing, name = "string_split" }

                "lines" ->
                    Just { moduleOrigin = Nothing, name = "string_lines" }

                "startsWith" ->
                    Just { moduleOrigin = Nothing, name = "string_startsWith" }

                "endsWith" ->
                    Just { moduleOrigin = Nothing, name = "string_endsWith" }

                "toInt" ->
                    Just { moduleOrigin = Nothing, name = "string_toInt" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "string_toFloat" }

                "fromInt" ->
                    Just { moduleOrigin = Nothing, name = "string" }

                "fromFloat" ->
                    Just { moduleOrigin = Nothing, name = "string" }

                "contains" ->
                    Just { moduleOrigin = Nothing, name = "string_contains" }

                "fromChar" ->
                    Just { moduleOrigin = Nothing, name = "string_fromChar" }

                "cons" ->
                    Just { moduleOrigin = Nothing, name = "string_cons" }

                "slice" ->
                    Just { moduleOrigin = Nothing, name = "string_slice" }

                "toLower" ->
                    Just { moduleOrigin = Nothing, name = "string_toLower" }

                "toUpper" ->
                    Just { moduleOrigin = Nothing, name = "string_toUpper" }

                _ ->
                    Nothing

        [ "Char" ] ->
            case reference.name of
                "Char" ->
                    Just { moduleOrigin = Nothing, name = "char" }

                "toCode" ->
                    Just { moduleOrigin = Nothing, name = "float" }

                "fromCode" ->
                    Just { moduleOrigin = Nothing, name = "float" }

                "toLower" ->
                    Just { moduleOrigin = Just "System.Char", name = "toLower" }

                "toUpper" ->
                    Just { moduleOrigin = Just "System.Char", name = "toUpper" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "List" ->
                    Just { moduleOrigin = Nothing, name = "list" }

                "singleton" ->
                    Just { moduleOrigin = Just "List", name = "exactlyOne" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "List", name = "isEmpty" }

                "length" ->
                    Just { moduleOrigin = Just "List", name = "length" }

                "member" ->
                    Just { moduleOrigin = Nothing, name = "list_member" }

                "sum" ->
                    Just { moduleOrigin = Just "List", name = "sum" }

                "product" ->
                    Just { moduleOrigin = Nothing, name = "list_product" }

                "append" ->
                    Just { moduleOrigin = Just "List", name = "append" }

                "concat" ->
                    Just { moduleOrigin = Just "List", name = "join" }

                "reverse" ->
                    Just { moduleOrigin = Just "List", name = "rev" }

                "repeat" ->
                    Just { moduleOrigin = Just "List", name = "replicate" }

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

                "map2" ->
                    Just { moduleOrigin = Just "List", name = "map2" }

                "zip" ->
                    Just { moduleOrigin = Just "List", name = "zip" }

                "map3" ->
                    Just { moduleOrigin = Just "List", name = "map3" }

                "unzip" ->
                    Just { moduleOrigin = Just "List", name = "unzip" }

                "concatMap" ->
                    Just { moduleOrigin = Just "List", name = "collect" }

                "sort" ->
                    Just { moduleOrigin = Just "List", name = "sort" }

                "sortBy" ->
                    Just { moduleOrigin = Just "List", name = "sortBy" }

                "sortWith" ->
                    Just { moduleOrigin = Nothing, name = "list_sortWith" }

                "range" ->
                    Just { moduleOrigin = Nothing, name = "list_range" }

                "take" ->
                    Just { moduleOrigin = Nothing, name = "list_take" }

                "drop" ->
                    Just { moduleOrigin = Nothing, name = "list_drop" }

                "intersperse" ->
                    Just { moduleOrigin = Nothing, name = "list_intersperse" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "list_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "list_foldr" }

                _ ->
                    Nothing

        [ "Dict" ] ->
            case reference.name of
                "Dict" ->
                    Just { moduleOrigin = Nothing, name = "Map" }

                "size" ->
                    Just { moduleOrigin = Just "Map", name = "count" }

                "empty" ->
                    Just { moduleOrigin = Just "Map", name = "empty" }

                "singleton" ->
                    Just { moduleOrigin = Nothing, name = "dict_singleton" }

                "fromList" ->
                    Just { moduleOrigin = Just "Map", name = "ofList" }

                "toList" ->
                    Just { moduleOrigin = Just "Map", name = "toList" }

                "keys" ->
                    Just { moduleOrigin = Nothing, name = "dict_keys" }

                "values" ->
                    Just { moduleOrigin = Nothing, name = "dict_values" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "Map", name = "isEmpty" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "dictMap" }

                "partition" ->
                    Just { moduleOrigin = Just "Map", name = "partition" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "dict_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "dict_foldr" }

                "filter" ->
                    Just { moduleOrigin = Just "Map", name = "filter" }

                "get" ->
                    Just { moduleOrigin = Just "Map", name = "tryFind" }

                "member" ->
                    Just { moduleOrigin = Just "Map", name = "containsKey" }

                "insert" ->
                    Just { moduleOrigin = Just "Map", name = "add" }

                "remove" ->
                    Just { moduleOrigin = Just "Map", name = "remove" }

                "union" ->
                    Just { moduleOrigin = Nothing, name = "dict_union" }

                "diff" ->
                    Just { moduleOrigin = Nothing, name = "dict_diff" }

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                -- refers to either a type or variant
                "Problem" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Problem" }

                "Expecting" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Expecting" }

                "ExpectingInt" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingInt" }

                "ExpectingHex" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingHex" }

                "ExpectingOctal" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingOctal" }

                "ExpectingBinary" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingBinary" }

                "ExpectingFloat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingFloat" }

                "ExpectingNumber" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingNumber" }

                "ExpectingVariable" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingVariable" }

                "ExpectingSymbol" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingSymbol" }

                "ExpectingKeyword" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingKeyword" }

                "ExpectingEnd" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingEnd" }

                "UnexpectedChar" ->
                    Just { moduleOrigin = Nothing, name = "Parser_UnexpectedChar" }

                "BadRepeat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_BadRepeat" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Maybe" ->
                    Just { moduleOrigin = Nothing, name = "option" }

                "Nothing" ->
                    Just { moduleOrigin = Nothing, name = "None" }

                "Just" ->
                    Just { moduleOrigin = Nothing, name = "Some" }

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
    (if reference.name |> stringFirstCharIsUpper then
        reference.moduleOrigin |> String.concat

     else
        reference.moduleOrigin |> String.concat |> stringFirstCharToLower
    )
        ++ "_"
        ++ reference.name


printFsharpPatternNotParenthesized : FsharpPattern -> Print
printFsharpPatternNotParenthesized fsharpPattern =
    -- IGNORE TCO
    case fsharpPattern of
        FsharpPatternIgnore ->
            printExactlyUnderscore

        FsharpPatternFloat floatValue ->
            Print.exactly (floatLiteral floatValue)

        FsharpPatternChar charValue ->
            Print.exactly (charLiteral charValue)

        FsharpPatternString string ->
            stringLiteral string

        FsharpPatternVariable name ->
            Print.exactly name

        FsharpPatternListCons fsharpPatternListCons ->
            printFsharpPatternListCons fsharpPatternListCons

        FsharpPatternListExact elements ->
            printFsharpPatternListExact elements

        FsharpPatternVariant patternVariant ->
            Print.exactly patternVariant.name
                |> Print.followedBy
                    (case patternVariant.value of
                        Nothing ->
                            Print.empty

                        Just variantValue ->
                            Print.exactly " "
                                |> Print.followedBy
                                    (printFsharpPatternNotParenthesizedIfSpaceSeparated
                                        variantValue
                                    )
                    )

        FsharpPatternAs patternAs ->
            printFsharpPatternAs patternAs

        FsharpPatternTuple parts ->
            Print.exactly "( "
                |> Print.followedBy
                    ((parts.part0 :: parts.part1 :: parts.part2Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            printFsharpPatternNotParenthesized
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly " )")


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


printFsharpPatternListCons :
    { head : FsharpPattern
    , tail : FsharpPattern
    }
    -> Print
printFsharpPatternListCons syntaxCons =
    printFsharpPatternNotParenthesizedIfSpaceSeparated
        syntaxCons.head
        |> Print.followedBy
            (Print.exactly " :: ")
        |> Print.followedBy
            (printFsharpPatternNotParenthesizedIfSpaceSeparated
                syntaxCons.tail
            )


printFsharpPatternAs :
    { variable : String
    , pattern : FsharpPattern
    }
    -> Print
printFsharpPatternAs syntaxAs =
    printFsharpPatternNotParenthesizedIfSpaceSeparated
        syntaxAs.pattern
        |> Print.followedBy
            (Print.exactly (" as " ++ syntaxAs.variable))


printFsharpExpressionRecord : FastDict.Dict String FsharpExpression -> Print
printFsharpExpressionRecord syntaxRecordFields =
    if syntaxRecordFields |> FastDict.isEmpty then
        Print.exactly "{||}"

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
                            Print.withIndentIncreasedBy 3
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
                                (Print.exactly ";  ")
                        )
        in
        printExactlyCurlyOpeningVerticalBarSpace
            |> Print.followedBy fieldsPrint
            |> Print.followedBy
                (Print.spaceOrLinebreakIndented
                    (fieldsPrint |> Print.lineSpread)
                )
            |> Print.followedBy printExactlyVerticalBarCurlyClosing


printParenthesized : Print -> Print
printParenthesized notParenthesizedPrint =
    let
        lineSpread : Print.LineSpread
        lineSpread =
            notParenthesizedPrint
                |> Print.lineSpread
    in
    printExactlyParensOpening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                notParenthesizedPrint
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


{-| Transpile a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
across multiple modules to [`FsharpDeclaration`]s.
Declarations that use unsupported stuff like parser kernel code (directly or indirectly)
will not be present in the final declarations.
Their errors can be found alongside the valid transpiled declarations.

The given list of files must also include files from used dependencies
including `elm/core`.

-}
modules :
    List Elm.Syntax.File.File
    ->
        { errors : List String
        , declarations : List FsharpDeclaration
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

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
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

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
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

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
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

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
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
                                                    }

                                                Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (typeAlias.name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    -- not supported
                                                    membersSoFar

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    -- invalid syntax
                                                    membersSoFar
                                        )
                                        { valueOrFunctionOrTypeAliasNames = FastSet.empty
                                        , choiceTypesExposingVariants = FastDict.empty
                                        }
                                )
                    )
                    FastDict.empty
    in
    syntaxModules
        |> List.foldr
            (\syntaxModule soFarAcrossModules ->
                let
                    moduleName : Elm.Syntax.ModuleName.ModuleName
                    moduleName =
                        syntaxModule.moduleDefinition
                            |> Elm.Syntax.Node.value
                            |> moduleHeaderName

                    createdModuleContext : ModuleContext
                    createdModuleContext =
                        moduleContextMerge
                            (syntaxModule.imports |> importsToModuleContext moduleMembers)
                            (case moduleMembers |> FastDict.get moduleName of
                                Nothing ->
                                    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                        FastDict.empty
                                    , variantLookup = FastDict.empty
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
                                    }
                            )
                in
                syntaxModule.declarations
                    |> List.foldr
                        (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                            case declaration of
                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                    case syntaxValueOrFunctionDeclaration |> valueOrFunctionDeclaration createdModuleContext of
                                        Ok fsharpValueOrFunctionDeclaration ->
                                            { errors = soFar.errors
                                            , declarations =
                                                FsharpDeclarationValueOrFunction
                                                    { fsharpValueOrFunctionDeclaration
                                                        | name =
                                                            { moduleOrigin = moduleName
                                                            , name = fsharpValueOrFunctionDeclaration.name
                                                            }
                                                                |> referenceToFsharpName
                                                    }
                                                    :: soFar.declarations
                                            }

                                        Err error ->
                                            { declarations = soFar.declarations
                                            , errors = error :: soFar.errors
                                            }

                                Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
                                    case syntaxTypeAliasDeclaration |> typeAliasDeclaration createdModuleContext of
                                        Ok fsharpTypeAliasDeclaration ->
                                            { errors = soFar.errors
                                            , declarations =
                                                FsharpDeclarationTypeAlias
                                                    { fsharpTypeAliasDeclaration
                                                        | name =
                                                            { moduleOrigin = moduleName
                                                            , name = fsharpTypeAliasDeclaration.name
                                                            }
                                                                |> referenceToFsharpName
                                                    }
                                                    :: soFar.declarations
                                            }

                                        Err error ->
                                            { declarations = soFar.declarations
                                            , errors = error :: soFar.errors
                                            }

                                Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
                                    case syntaxChoiceTypeDeclaration.name |> Elm.Syntax.Node.value of
                                        "Maybe" ->
                                            soFar

                                        _ ->
                                            case syntaxChoiceTypeDeclaration |> choiceTypeDeclaration createdModuleContext of
                                                Ok fsharpTypeAliasDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        FsharpDeclarationChoiceType
                                                            { fsharpTypeAliasDeclaration
                                                                | name =
                                                                    { moduleOrigin = moduleName
                                                                    , name = fsharpTypeAliasDeclaration.name
                                                                    }
                                                                        |> referenceToFsharpName
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
                                                            :: soFar.declarations
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
            , declarations = []
            }


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
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , expression : FsharpExpression
            , type_ : Maybe FsharpType
            }
valueOrFunctionDeclaration moduleOriginLookup syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            let
                fieldVariablesToIntroduce : List { field : String, recordVariable : String }
                fieldVariablesToIntroduce =
                    parameters
                        |> List.concatMap .fieldVariablesToIntroduce
            in
            Result.map
                (\result ->
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> variableNameDisambiguateFromFsharpKeywords
                    , type_ = maybeType
                    , expression =
                        case parameters of
                            [] ->
                                result

                            parameter0 :: parameter1Up ->
                                FsharpExpressionLambda
                                    { parameter0 = parameter0.pattern
                                    , parameter1Up =
                                        parameter1Up
                                            |> List.map .pattern
                                    , result =
                                        result
                                            |> introduceFieldVariableLetDestructurings
                                                fieldVariablesToIntroduce
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            moduleOriginLookup.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                        , variantLookup =
                            moduleOriginLookup.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            FastSet.union
                                (fieldVariablesToIntroduce
                                    |> listMapAndToFastSet
                                        (\fieldVariableToIntroduce ->
                                            fieldVariableToIntroduce.field
                                                |> stringFirstCharToLower
                                                |> variableNameDisambiguateFromFsharpKeywords
                                        )
                                )
                                (parameters
                                    |> listMapToFastSetsAndUnify .introducedVariables
                                )
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk (\p -> p |> pattern moduleOriginLookup)
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation |> type_ moduleOriginLookup)
        )


introduceFieldVariableLetDestructurings :
    List { field : String, recordVariable : String }
    -> FsharpExpression
    -> FsharpExpression
introduceFieldVariableLetDestructurings introducedFieldVariables result =
    case introducedFieldVariables of
        [] ->
            result

        introducedFieldVariable0 :: introducedFieldVariable1Up ->
            FsharpExpressionWithLetDeclarations
                { declaration0 =
                    FsharpLetDeclarationValueOrFunction
                        (introducedFieldVariable0
                            |> introducedFieldVariableToLetValue
                        )
                , declaration1Up =
                    introducedFieldVariable1Up
                        |> List.map
                            (\fieldVariableToIntroduce ->
                                FsharpLetDeclarationValueOrFunction
                                    (fieldVariableToIntroduce
                                        |> introducedFieldVariableToLetValue
                                    )
                            )
                , result = result
                }


introducedFieldVariableToLetValue :
    { field : String, recordVariable : String }
    ->
        { name : String
        , type_ : Maybe FsharpType
        , expression : FsharpExpression
        }
introducedFieldVariableToLetValue introducedFieldVariable =
    { name =
        introducedFieldVariable.field
            |> stringFirstCharToLower
            |> variableNameDisambiguateFromFsharpKeywords
    , type_ = Nothing
    , expression =
        FsharpExpressionRecordAccess
            { record =
                FsharpExpressionReference
                    { moduleOrigin = Nothing
                    , name = introducedFieldVariable.recordVariable
                    }
            , field = introducedFieldVariable.field
            }
    }


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
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
expressionContextAddVariablesInScope additionalVariablesInScope context =
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
        context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
    , variantLookup =
        context.variantLookup
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
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Result String FsharpExpression
expression context (Elm.Syntax.Node.Node _ syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok FsharpExpressionUnit

        Elm.Syntax.Expression.Integer intValue ->
            Ok (FsharpExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Hex intValue ->
            Ok (FsharpExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Floatable floatValue ->
            Ok (FsharpExpressionFloat floatValue)

        Elm.Syntax.Expression.CharLiteral charValue ->
            Ok (FsharpExpressionChar charValue)

        Elm.Syntax.Expression.Literal stringValue ->
            Ok (FsharpExpressionString stringValue)

        Elm.Syntax.Expression.RecordAccessFunction fieldName ->
            Ok
                (FsharpExpressionRecordAccessFunction
                    (fieldName
                        |> String.replace "." ""
                        |> stringFirstCharToUpper
                    )
                )

        Elm.Syntax.Expression.Operator _ ->
            -- invalid syntax
            Err "operator is invalid syntax"

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Result.map
                (\operationFunctionReference ->
                    FsharpExpressionReference operationFunctionReference
                )
                (expressionOperatorToFsharpFunctionReference operatorSymbol)

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl not supported"

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "application without any parts is invalid"

                [ inParens ] ->
                    -- invalid syntax
                    expression context inParens

                calledNode :: argument0Node :: argument1UpNodes ->
                    Result.map3
                        (\called argument0 argument1Up ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument0
                                , argument1Up = argument1Up
                                }
                        )
                        (calledNode |> expression context)
                        (argument0Node |> expression context)
                        (argument1UpNodes
                            |> listMapAndCombineOk
                                (\argument -> argument |> expression context)
                        )

        Elm.Syntax.Expression.OperatorApplication operatorSymbol _ leftNode rightNode ->
            case operatorSymbol of
                "|>" ->
                    Result.map2
                        (\argument called ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "<|" ->
                    Result.map2
                        (\called argument ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "++" ->
                    Result.map2
                        (\left right ->
                            if
                                (left |> fsharpExpressionIsDefinitelyOfTypeString)
                                    || (right |> fsharpExpressionIsDefinitelyOfTypeString)
                            then
                                FsharpExpressionCall
                                    { called =
                                        FsharpExpressionReference
                                            { moduleOrigin = Nothing
                                            , name = "string_append"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }

                            else
                                FsharpExpressionCall
                                    { called =
                                        FsharpExpressionReference
                                            { moduleOrigin = Just "List"
                                            , name = "append"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                otherOperatorSymbol ->
                    Result.map3
                        (\operationFunctionReference left right ->
                            FsharpExpressionCall
                                { called =
                                    FsharpExpressionReference operationFunctionReference
                                , argument0 = left
                                , argument1Up = [ right ]
                                }
                        )
                        (expressionOperatorToFsharpFunctionReference otherOperatorSymbol)
                        (leftNode |> expression context)
                        (rightNode |> expression context)

        Elm.Syntax.Expression.FunctionOrValue qualification name ->
            let
                isVariableFromWithinDeclaration : Bool
                isVariableFromWithinDeclaration =
                    case qualification of
                        _ :: _ ->
                            False

                        [] ->
                            context.variablesFromWithinDeclarationInScope
                                |> FastSet.member name
            in
            if isVariableFromWithinDeclaration then
                Ok
                    (FsharpExpressionReference
                        { moduleOrigin = Nothing
                        , name = name |> variableNameDisambiguateFromFsharpKeywords
                        }
                    )

            else
                case context.variantLookup |> FastDict.get ( qualification, name ) of
                    Just variantInfo ->
                        let
                            reference : { moduleOrigin : Maybe String, name : String }
                            reference =
                                case { moduleOrigin = variantInfo.moduleOrigin, name = name } |> referenceToCoreFsharp of
                                    Just fsharpReference ->
                                        fsharpReference

                                    Nothing ->
                                        { moduleOrigin = Nothing
                                        , name =
                                            referenceToFsharpName
                                                { moduleOrigin = variantInfo.moduleOrigin
                                                , name = name
                                                }
                                        }
                        in
                        Ok
                            (case variantInfo.valueCount of
                                0 ->
                                    FsharpExpressionReference reference

                                1 ->
                                    FsharpExpressionReference reference

                                valueCountAtLeast2 ->
                                    let
                                        generatedValueVariableReference : Int -> FsharpExpression
                                        generatedValueVariableReference valueIndex =
                                            FsharpExpressionReference
                                                { moduleOrigin = Nothing
                                                , name =
                                                    "generated_variantParameter"
                                                        ++ (valueIndex |> String.fromInt)
                                                }

                                        generatedValuePattern : Int -> FsharpPattern
                                        generatedValuePattern valueIndex =
                                            FsharpPatternVariable
                                                ("generated_variantParameter"
                                                    ++ (valueIndex |> String.fromInt)
                                                )
                                    in
                                    FsharpExpressionLambda
                                        { parameter0 = generatedValuePattern 0
                                        , parameter1Up =
                                            generatedValuePattern 1
                                                :: (List.range 2 (valueCountAtLeast2 - 1)
                                                        |> List.map generatedValuePattern
                                                   )
                                        , result =
                                            FsharpExpressionCall
                                                { called = FsharpExpressionReference reference
                                                , argument0 =
                                                    FsharpExpressionTuple
                                                        { part0 = generatedValueVariableReference 0
                                                        , part1 = generatedValueVariableReference 1
                                                        , part2Up =
                                                            List.range 2 (valueCountAtLeast2 - 1)
                                                                |> List.map generatedValueVariableReference
                                                        }
                                                , argument1Up = []
                                                }
                                        }
                            )

                    Nothing ->
                        case context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup |> FastDict.get ( qualification, name ) of
                            Just moduleOrigin ->
                                Ok
                                    (FsharpExpressionReference
                                        (case { moduleOrigin = moduleOrigin, name = name } |> referenceToCoreFsharp of
                                            Just fsharpReference ->
                                                fsharpReference

                                            Nothing ->
                                                { moduleOrigin = Nothing
                                                , name =
                                                    referenceToFsharpName
                                                        { moduleOrigin = moduleOrigin
                                                        , name = name
                                                        }
                                                }
                                        )
                                    )

                            Nothing ->
                                case qualification of
                                    qualificationPart0 :: qualificationPart1Up ->
                                        Err
                                            ("could not find module origin of the qualified reference "
                                                ++ (((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                                                        ++ "."
                                                        ++ name
                                                   )
                                            )

                                    [] ->
                                        -- TODO convert to error
                                        Ok
                                            (FsharpExpressionReference
                                                { moduleOrigin = Nothing
                                                , name = name |> variableNameDisambiguateFromFsharpKeywords
                                                }
                                            )

        Elm.Syntax.Expression.IfBlock conditionNode onTrueNode onFalseNode ->
            Result.map3
                (\condition onTrue onFalse ->
                    FsharpExpressionIfThenElse
                        { condition = condition
                        , onTrue = onTrue
                        , onFalse = onFalse
                        }
                )
                (conditionNode |> expression context)
                (onTrueNode |> expression context)
                (onFalseNode |> expression context)

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expression context

        Elm.Syntax.Expression.Negation inNegationNode ->
            Result.map
                (\inNegation ->
                    FsharpExpressionCall
                        { called =
                            FsharpExpressionReference
                                { moduleOrigin = Nothing, name = "basics_negate" }
                        , argument0 = inNegation
                        , argument1Up = []
                        }
                )
                (inNegationNode |> expression context)

        Elm.Syntax.Expression.RecordAccess recordNode (Elm.Syntax.Node.Node _ fieldName) ->
            Result.map
                (\record ->
                    FsharpExpressionRecordAccess
                        { record = record
                        , field =
                            fieldName
                                |> String.replace "." ""
                                |> stringFirstCharToUpper
                        }
                )
                (recordNode |> expression context)

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.UnitExpr
                    Ok FsharpExpressionUnit

                [ inParens ] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.ParenthesizedExpression
                    expression context inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            FsharpExpressionTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            FsharpExpressionTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)
                        (part2Node |> expression context)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.Expression.ListExpr elementNodes ->
            Result.map (\elements -> FsharpExpressionList elements)
                (elementNodes
                    |> listMapAndCombineOk
                        (\element -> element |> expression context)
                )

        Elm.Syntax.Expression.RecordExpr fieldNodes ->
            Result.map (\fields -> FsharpExpressionRecord fields)
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName |> stringFirstCharToUpper
                                    , fieldValue
                                    )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ originalRecordVariable) fieldNodes ->
            Result.map
                (\fields ->
                    FsharpExpressionRecordUpdate
                        { originalRecordVariable =
                            referenceToFsharpName
                                { moduleOrigin =
                                    case context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup |> FastDict.get ( [], originalRecordVariable ) of
                                        Nothing ->
                                            []

                                        Just moduleOrigin ->
                                            moduleOrigin
                                , name = originalRecordVariable |> variableNameDisambiguateFromFsharpKeywords
                                }
                        , fields = fields
                        }
                )
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName |> stringFirstCharToUpper
                                    , fieldValue
                                    )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.LambdaExpression lambda ->
            case lambda.args of
                [] ->
                    Err "lambda without parameters is invalid syntax"

                parameter0Node :: parameter1UpNodes ->
                    resultAndThen2
                        (\parameter0 parameter1Up ->
                            let
                                fieldVariablesToIntroduce : List { field : String, recordVariable : String }
                                fieldVariablesToIntroduce =
                                    (parameter0 :: parameter1Up)
                                        |> List.concatMap .fieldVariablesToIntroduce
                            in
                            Result.map
                                (\result ->
                                    FsharpExpressionLambda
                                        { parameter0 = parameter0.pattern
                                        , parameter1Up =
                                            parameter1Up |> List.map .pattern
                                        , result =
                                            result
                                                |> introduceFieldVariableLetDestructurings
                                                    fieldVariablesToIntroduce
                                        }
                                )
                                (lambda.expression
                                    |> expression
                                        (context
                                            |> expressionContextAddVariablesInScope
                                                (FastSet.union
                                                    (fieldVariablesToIntroduce
                                                        |> listMapAndToFastSet
                                                            (\fieldVariableToIntroduce ->
                                                                fieldVariableToIntroduce.field
                                                                    |> stringFirstCharToLower
                                                                    |> variableNameDisambiguateFromFsharpKeywords
                                                            )
                                                    )
                                                    (FastSet.union
                                                        parameter0.introducedVariables
                                                        (parameter1Up
                                                            |> listMapToFastSetsAndUnify .introducedVariables
                                                        )
                                                    )
                                                )
                                        )
                                )
                        )
                        (parameter0Node
                            |> pattern
                                { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                    context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                                , variantLookup = context.variantLookup
                                }
                        )
                        (parameter1UpNodes
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter
                                        |> pattern
                                            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                                context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                                            , variantLookup = context.variantLookup
                                            }
                                )
                        )

        Elm.Syntax.Expression.CaseExpression caseOf ->
            case caseOf.cases of
                [] ->
                    Err "case-of without cases invalid syntax"

                case0Node :: case1Node ->
                    Result.map3
                        (\matched case0 case1Up ->
                            FsharpExpressionMatchWith
                                { matched = matched
                                , case0 = case0
                                , case1Up = case1Up
                                }
                        )
                        (caseOf.expression |> expression context)
                        (case0Node |> case_ context)
                        (case1Node
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> case_ context
                                )
                        )

        Elm.Syntax.Expression.LetExpression letIn ->
            case letIn.declarations of
                [] ->
                    Err "let-in without declarations is invalid syntax"

                declaration0Node :: declaration1UpNode ->
                    let
                        variablesForWholeLetIn : FastSet.Set String
                        variablesForWholeLetIn =
                            (declaration0Node :: declaration1UpNode)
                                |> listMapToFastSetsAndUnify
                                    (\(Elm.Syntax.Node.Node _ syntaxLetDeclaration) ->
                                        case syntaxLetDeclaration of
                                            Elm.Syntax.Expression.LetFunction letFunction ->
                                                FastSet.singleton
                                                    (letFunction.declaration
                                                        |> Elm.Syntax.Node.value
                                                        |> .name
                                                        |> Elm.Syntax.Node.value
                                                        |> variableNameDisambiguateFromFsharpKeywords
                                                    )

                                            Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                                patternNode
                                                    |> patternBindings
                                                    |> listMapAndToFastSet variableNameDisambiguateFromFsharpKeywords
                                    )
                    in
                    Result.map3
                        (\declaration0 declaration1Up result ->
                            FsharpExpressionWithLetDeclarations
                                { declaration0 = declaration0.main
                                , declaration1Up =
                                    ((declaration0 :: declaration1Up)
                                        |> List.concatMap .fieldVariablesToIntroduce
                                        |> List.map
                                            (\fieldVariableToIntroduce ->
                                                FsharpLetDeclarationValueOrFunction
                                                    (fieldVariableToIntroduce
                                                        |> introducedFieldVariableToLetValue
                                                    )
                                            )
                                    )
                                        ++ (declaration1Up
                                                |> List.map .main
                                           )
                                , result = result
                                }
                        )
                        -- destructuring patterns currently do not accept as
                        -- https://github.com/fsharp-lang/fsharp/issues/7328
                        (declaration0Node
                            |> letDeclaration
                                (context
                                    |> expressionContextAddVariablesInScope
                                        variablesForWholeLetIn
                                )
                        )
                        (declaration1UpNode
                            |> listMapAndCombineOk
                                (\letDecl ->
                                    letDecl
                                        |> letDeclaration
                                            (context
                                                |> expressionContextAddVariablesInScope
                                                    variablesForWholeLetIn
                                            )
                                )
                        )
                        (letIn.expression
                            |> expression
                                (context
                                    |> expressionContextAddVariablesInScope
                                        variablesForWholeLetIn
                                )
                        )


{-| Recursively find all introduced variables
in the [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)
(like `a` and `b` in `( Just a, { b } )`)
-}
patternBindings : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.VarPattern name ->
            [ name ]

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            name :: (afterAsPattern |> patternBindings)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternBindings

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.RecordPattern fields ->
            fields |> List.map Elm.Syntax.Node.value

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            (tailPattern |> patternBindings) ++ (headPattern |> patternBindings)

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
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
    (a -> FastSet.Set comparable)
    -> List a
    -> FastSet.Set comparable
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
            condenseExpressionCall
                { called = calledCall.called
                , argument0 = calledCall.argument0
                , argument1Up =
                    calledCall.argument1Up
                        ++ (call.argument0 :: call.argument1Up)
                }

        FsharpExpressionRecordAccessFunction field ->
            case call.argument1Up of
                [] ->
                    FsharpExpressionRecordAccess
                        { record = call.argument0
                        , field = field
                        }

                argument1 :: argument2Up ->
                    FsharpExpressionCall
                        { called =
                            FsharpExpressionRecordAccess
                                { record = call.argument0
                                , field = field
                                }
                        , argument0 = argument1
                        , argument1Up = argument2Up
                        }

        calledNotCall ->
            FsharpExpressionCall
                { called = calledNotCall
                , argument0 = call.argument0
                , argument1Up = call.argument1Up
                }


fsharpExpressionIsDefinitelyOfTypeString : FsharpExpression -> Bool
fsharpExpressionIsDefinitelyOfTypeString fsharpExpression =
    case fsharpExpression of
        FsharpExpressionString _ ->
            True

        FsharpExpressionCall call ->
            call.called
                == FsharpExpressionReference { moduleOrigin = Nothing, name = "string_append" }
                && ((call.argument1Up |> List.length) == 1)

        FsharpExpressionUnit ->
            False

        FsharpExpressionChar _ ->
            False

        FsharpExpressionFloat _ ->
            False

        FsharpExpressionReference _ ->
            False

        FsharpExpressionRecordAccessFunction _ ->
            False

        FsharpExpressionRecordAccess _ ->
            False

        FsharpExpressionTuple _ ->
            False

        FsharpExpressionIfThenElse _ ->
            False

        FsharpExpressionList _ ->
            False

        FsharpExpressionRecord _ ->
            False

        FsharpExpressionRecordUpdate _ ->
            False

        FsharpExpressionLambda _ ->
            False

        FsharpExpressionMatchWith _ ->
            False

        FsharpExpressionWithLetDeclarations _ ->
            False


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
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { pattern : FsharpPattern, result : FsharpExpression }
case_ context ( patternNode, resultNode ) =
    Result.andThen
        (\casePattern ->
            Result.map
                (\result ->
                    { pattern = casePattern.pattern
                    , result =
                        result
                            |> introduceFieldVariableLetDestructurings
                                casePattern.fieldVariablesToIntroduce
                    }
                )
                (resultNode
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                (FastSet.union
                                    (casePattern.fieldVariablesToIntroduce
                                        |> listMapAndToFastSet
                                            (\fieldVariableToIntroduce ->
                                                fieldVariableToIntroduce.field
                                                    |> stringFirstCharToLower
                                                    |> variableNameDisambiguateFromFsharpKeywords
                                            )
                                    )
                                    casePattern.introducedVariables
                                )
                        )
                )
        )
        (patternNode
            |> pattern
                { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                    context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                , variantLookup = context.variantLookup
                }
        )


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
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    ->
        Result
            String
            { main : FsharpLetDeclaration
            , fieldVariablesToIntroduce :
                List
                    { recordVariable : String
                    , field : String
                    }
            }
letDeclaration context (Elm.Syntax.Node.Node _ syntaxLetDeclaration) =
    case syntaxLetDeclaration of
        Elm.Syntax.Expression.LetDestructuring destructuringPatternNode destructuringExpressionNode ->
            Result.map2
                (\destructuringPattern destructuringExpression ->
                    { main =
                        FsharpLetDestructuring
                            { pattern = destructuringPattern.pattern
                            , expression = destructuringExpression
                            }
                    , fieldVariablesToIntroduce =
                        destructuringPattern.fieldVariablesToIntroduce
                    }
                )
                (destructuringPatternNode
                    |> pattern
                        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                        , variantLookup = context.variantLookup
                        }
                )
                (destructuringExpressionNode |> expression context)

        Elm.Syntax.Expression.LetFunction letValueOrFunction ->
            Result.map
                (\valueOrFunction ->
                    { main =
                        FsharpLetDeclarationValueOrFunction
                            valueOrFunction
                    , fieldVariablesToIntroduce = []
                    }
                )
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
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , expression : FsharpExpression
            , type_ : Maybe FsharpType
            }
letValueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            let
                fieldVariablesToIntroduce : List { field : String, recordVariable : String }
                fieldVariablesToIntroduce =
                    parameters
                        |> List.concatMap .fieldVariablesToIntroduce
            in
            Result.map
                (\result ->
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> variableNameDisambiguateFromFsharpKeywords
                    , type_ = maybeType
                    , expression =
                        case parameters of
                            [] ->
                                result

                            parameter0 :: parameter1Up ->
                                FsharpExpressionLambda
                                    { parameter0 = parameter0.pattern
                                    , parameter1Up =
                                        parameter1Up
                                            |> List.map .pattern
                                    , result =
                                        result
                                            |> introduceFieldVariableLetDestructurings
                                                fieldVariablesToIntroduce
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                        , variantLookup =
                            context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            FastSet.union
                                (FastSet.union
                                    (fieldVariablesToIntroduce
                                        |> listMapAndToFastSet
                                            (\fieldVariableToIntroduce ->
                                                fieldVariableToIntroduce.field
                                                    |> stringFirstCharToLower
                                                    |> variableNameDisambiguateFromFsharpKeywords
                                            )
                                    )
                                    (parameters
                                        |> listMapToFastSetsAndUnify .introducedVariables
                                    )
                                )
                                context.variablesFromWithinDeclarationInScope
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                    )
        )


expressionOperatorToFsharpFunctionReference :
    String
    -> Result String { moduleOrigin : Maybe String, name : String }
expressionOperatorToFsharpFunctionReference operatorSymbol =
    case operatorSymbol of
        "+" ->
            Ok { moduleOrigin = Nothing, name = "basics_add" }

        "-" ->
            Ok { moduleOrigin = Nothing, name = "basics_sub" }

        "*" ->
            Ok { moduleOrigin = Nothing, name = "basics_mul" }

        "/" ->
            Ok { moduleOrigin = Nothing, name = "basics_fdiv" }

        "//" ->
            Ok { moduleOrigin = Nothing, name = "basics_idiv" }

        "^" ->
            Ok { moduleOrigin = Nothing, name = "basics_pow" }

        "==" ->
            Ok { moduleOrigin = Nothing, name = "basics_eq" }

        "/=" ->
            Ok { moduleOrigin = Nothing, name = "basics_neq" }

        "||" ->
            Ok { moduleOrigin = Nothing, name = "basics_or" }

        "&&" ->
            Ok { moduleOrigin = Nothing, name = "basics_and" }

        "<" ->
            Ok { moduleOrigin = Nothing, name = "basics_lt" }

        ">" ->
            Ok { moduleOrigin = Nothing, name = "basics_gt" }

        "<=" ->
            Ok { moduleOrigin = Nothing, name = "basics_le" }

        ">=" ->
            Ok { moduleOrigin = Nothing, name = "basics_ge" }

        "::" ->
            Ok { moduleOrigin = Nothing, name = "list_cons" }

        "++" ->
            Ok { moduleOrigin = Just "List", name = "append" }

        unknownOrUnsupportedOperator ->
            Err ("unknown/unsupported operator " ++ unknownOrUnsupportedOperator)


{-| Print a fsharp value/function declaration
-}
printFsharpValueOrFunctionDeclaration :
    { name : String
    , expression : FsharpExpression
    , type_ : Maybe FsharpType
    }
    -> Print
printFsharpValueOrFunctionDeclaration fsharpValueOrFunctionDeclaration =
    Print.exactly ("let " ++ fsharpValueOrFunctionDeclaration.name)
        |> Print.followedBy
            (case fsharpValueOrFunctionDeclaration.type_ of
                Nothing ->
                    Print.empty

                Just letType ->
                    let
                        typePrint : Print
                        typePrint =
                            printFsharpTypeNotParenthesized letType
                    in
                    Print.exactly ":"
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.withIndentAtNextMultipleOf4
                                    (Print.spaceOrLinebreakIndented
                                        (typePrint |> Print.lineSpread)
                                        |> Print.followedBy typePrint
                                    )
                                )
                            )
            )
        |> Print.followedBy
            (Print.exactly " =")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (fsharpValueOrFunctionDeclaration.expression
                            |> printFsharpExpressionNotParenthesized
                        )
                )
            )


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
        printParenthesized (printFsharpExpressionNotParenthesized fsharpExpression)

    else
        printFsharpExpressionNotParenthesized fsharpExpression


fsharpExpressionIsSpaceSeparated : FsharpExpression -> Bool
fsharpExpressionIsSpaceSeparated fsharpExpression =
    case fsharpExpression of
        FsharpExpressionUnit ->
            False

        FsharpExpressionChar _ ->
            False

        FsharpExpressionFloat _ ->
            False

        FsharpExpressionString _ ->
            False

        FsharpExpressionReference _ ->
            False

        FsharpExpressionRecordAccessFunction _ ->
            False

        FsharpExpressionRecordAccess _ ->
            False

        FsharpExpressionTuple _ ->
            False

        FsharpExpressionIfThenElse _ ->
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

        FsharpExpressionIfThenElse ifThenElse ->
            printFsharpExpressionIfThenElse ifThenElse

        FsharpExpressionChar charValue ->
            Print.exactly (charLiteral charValue)

        FsharpExpressionFloat float ->
            Print.exactly (floatLiteral float)

        FsharpExpressionString string ->
            stringLiteral string

        FsharpExpressionRecordAccessFunction fieldName ->
            Print.exactly ("_." ++ fieldName)

        FsharpExpressionTuple parts ->
            let
                part0Print : Print
                part0Print =
                    parts.part0 |> printFsharpExpressionNotParenthesized

                part1Print : Print
                part1Print =
                    parts.part1 |> printFsharpExpressionNotParenthesized

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
            Print.exactly "( "
                |> Print.followedBy
                    ((part0Print :: part1Print :: part2UpPrints)
                        |> Print.listIntersperseAndFlatten
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy
                                    (Print.exactly ", ")
                            )
                    )
                |> Print.followedBy
                    (Print.spaceOrLinebreakIndented lineSpread)
                |> Print.followedBy (Print.exactly ")")

        FsharpExpressionWithLetDeclarations expressionWithLetDeclarations ->
            printFsharpExpressionWithLetDeclarations expressionWithLetDeclarations

        FsharpExpressionMatchWith syntaxWhenIs ->
            printFsharpExpressionMatchWith syntaxWhenIs

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


printFsharpExpressionCall :
    { called : FsharpExpression
    , argument0 : FsharpExpression
    , argument1Up : List FsharpExpression
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
            (call.argument0 :: call.argument1Up)
                |> List.map printFsharpExpressionParenthesizedIfSpaceSeparated

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
                                Print.withIndentIncreasedBy 2
                                    (element |> printFsharpExpressionNotParenthesized)
                            )
                            (Print.linebreakIndented
                                |> Print.followedBy (Print.exactly "; ")
                            )
            in
            Print.exactly "[ "
                |> Print.followedBy elementsPrint
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
    printExactlyCurlyOpeningVerticalBarSpace
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
                                    Print.withIndentIncreasedBy 3
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
                                        (Print.exactly ";  ")
                                )
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyVerticalBarCurlyClosing


patternIsSpaceSeparated : FsharpPattern -> Bool
patternIsSpaceSeparated fsharpPattern =
    case fsharpPattern of
        FsharpPatternIgnore ->
            False

        FsharpPatternFloat _ ->
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

        FsharpPatternVariant patternVariant ->
            case patternVariant.value of
                Nothing ->
                    False

                Just _ ->
                    True

        FsharpPatternTuple _ ->
            False


printFsharpPatternNotParenthesizedIfSpaceSeparated : FsharpPattern -> Print
printFsharpPatternNotParenthesizedIfSpaceSeparated fsharpPattern =
    if fsharpPattern |> patternIsSpaceSeparated then
        printParenthesized
            (fsharpPattern |> printFsharpPatternNotParenthesized)

    else
        fsharpPattern |> printFsharpPatternNotParenthesized


printFsharpExpressionLambda :
    { parameter0 : FsharpPattern
    , parameter1Up : List FsharpPattern
    , result : FsharpExpression
    }
    -> Print
printFsharpExpressionLambda syntaxLambda =
    printExactlyFunSpace
        |> Print.followedBy
            ((syntaxLambda.parameter0 :: syntaxLambda.parameter1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    printFsharpPatternNotParenthesizedIfSpaceSeparated
                    (Print.exactly " ")
            )
        |> Print.followedBy (Print.exactly " ->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionNotParenthesized syntaxLambda.result)
                )
            )


printFsharpExpressionIfThenElse :
    { condition : FsharpExpression
    , onTrue : FsharpExpression
    , onFalse : FsharpExpression
    }
    -> Print
printFsharpExpressionIfThenElse syntaxIfThenElse =
    let
        conditionPrint : Print
        conditionPrint =
            printFsharpExpressionNotParenthesized syntaxIfThenElse.condition

        conditionLineSpread : Print.LineSpread
        conditionLineSpread =
            conditionPrint |> Print.lineSpread
    in
    printExactlyIf
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
                        (printFsharpExpressionNotParenthesized syntaxIfThenElse.onTrue)
                    |> Print.followedBy Print.linebreak
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyElse
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionNotParenthesized syntaxIfThenElse.onFalse)
                )
            )


printFsharpExpressionMatchWith :
    { matched : FsharpExpression
    , case0 : { pattern : FsharpPattern, result : FsharpExpression }
    , case1Up : List { pattern : FsharpPattern, result : FsharpExpression }
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
    printExactlyMatch
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


printFsharpExpressionWithLetDeclarations :
    { declaration0 : FsharpLetDeclaration
    , declaration1Up : List FsharpLetDeclaration
    , result : FsharpExpression
    }
    -> Print
printFsharpExpressionWithLetDeclarations syntaxLetIn =
    (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
        |> Print.listMapAndIntersperseAndFlatten
            printFsharpLetDeclaration
            (Print.linebreakIndented
                |> Print.followedBy Print.linebreakIndented
            )
        |> Print.followedBy printLinebreakLinebreakIndented
        |> Print.followedBy
            (printFsharpExpressionNotParenthesized syntaxLetIn.result)


printFsharpLetDeclaration : FsharpLetDeclaration -> Print
printFsharpLetDeclaration fsharpLetDeclaration =
    case fsharpLetDeclaration of
        FsharpLetDeclarationValueOrFunction letDeclarationExpression ->
            printFsharpValueOrFunctionDeclaration letDeclarationExpression

        FsharpLetDestructuring letDestructuring ->
            Print.exactly "let "
                |> Print.followedBy
                    (printFsharpPatternNotParenthesizedIfSpaceSeparated letDestructuring.pattern)
                |> Print.followedBy (Print.exactly " =")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (printFsharpExpressionNotParenthesized letDestructuring.expression)
                        )
                    )


printFsharpExpressionMatchWithCase :
    { pattern : FsharpPattern, result : FsharpExpression }
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


{-| Print value/function declarations into
an F# module called `Elm` in the global namespace that exposes all members.
Will also add some internal wrapper declarations.
-}
fsharpDeclarationsToModuleString : List FsharpDeclaration -> String
fsharpDeclarationsToModuleString declarations =
    ("""namespace global

module rec Elm =
    let basics_always (result: 'result) (_: '_ignored) : 'result = result

    let basics_eq (a: 'a) (b: 'a) = a = b
    let basics_neq (a: 'a) (b: 'a) = a <> b
    let basics_lt (a: float) (b: float) : bool = a < b
    let basics_le (a: float) (b: float) : bool = a <= b
    let basics_gt (a: float) (b: float) : bool = a > b
    let basics_ge (a: float) (b: float) : bool = a >= b

    type Basics_Order =
        | Basics_LT
        | Basics_EQ
        | Basics_GT

    let basics_compare (a: 'a) (b: 'a) : Basics_Order =
        let comparisonMagnitude = compare a b

        if comparisonMagnitude = 0 then Basics_EQ
        else if comparisonMagnitude < 0 then Basics_LT
        else Basics_GT

    let basics_negate (float: float) : float = -float

    let basics_add (a: float) (b: float) : float = a + b

    let basics_sub (a: float) (b: float) : float = a - b

    let basics_mul (a: float) (b: float) : float = a * b

    let basics_fdiv (a: float) (b: float) : float = a / b

    let basics_idiv (a: float) (b: float) : float = truncate (a / b)

    let basics_remainderBy (divisor: float) (toDivide: float) : float =
        toDivide % divisor

    let basics_modBy (divisor: float) (toDivide: float) : float =
        let remainder = toDivide % divisor

        if
            (remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0)
        then
            remainder + toDivide


        else
            remainder

    let basics_pow (a: float) (b: float) : float = a ** b

    let basics_and (a: bool) (b: bool) = a && b

    let basics_or (a: bool) (b: bool) = a || b

    let string_isEmpty (stringToCheck: string) : bool = stringToCheck = ""

    let string_toList (string: string) : list<char> =
        List.ofArray (string.ToCharArray())

    let string_fromList (chars: list<char>) =
        new string (List.toArray chars)

    let string_contains (substring: string) (string: string) : bool =
        string.Contains(substring)

    let string_startsWith (start: string) (string: string) : bool =
        string.StartsWith(start)

    let string_endsWith (ending: string) (string: string) : bool =
        string.EndsWith(ending)

    let string_foldl
        (reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: string)
        : 'folded =
        Array.fold (fun soFar char -> reduce char soFar) initialFolded (string.ToCharArray())

    let string_foldr
        (reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: string)
        : 'folded =
        Array.foldBack reduce (string.ToCharArray()) initialFolded

    let string_trim (string: string) : string = string.Trim()

    let string_trimLeft (string: string) : string = string.TrimStart()

    let string_trimRight (string: string) : string = string.TrimEnd()

    let string_right (takenElementCount: float) (string: string): string = 
        string.Substring(
            String.length string - int takenElementCount - 1,
            int takenElementCount
        )

    let string_left (skippedElementCount: float) (string: string): string = 
        string.Substring(
            0,
            int skippedElementCount
        )
    
    let string_dropRight (skippedElementCount: float) (string: string): string = 
        string.Substring(
            0,
            String.length string - int skippedElementCount
        )

    let string_dropLeft (skippedElementCount: float) (string: string): string = 
        string.Substring(
            int skippedElementCount - 1,
            String.length string - int skippedElementCount
        )

    let string_append (early: string) (late: string) : string = early + late

    let string_fromChar (char: char) : string = string char

    let string_cons (newHeadChar: char) (late: string) : string =
        string_fromChar newHeadChar + late

    let string_split (separator: string) (string: string) : list<string> =
        List.ofArray (string.Split(separator))

    let string_lines (string: string) : list<string> =
        List.ofArray (
            string
                .Replace("\\r\\n", "\\n")
                .Split("\\n")
        )

    let string_reverse (string: string) : string =
        new string (Array.rev (string.ToCharArray()))

    let string_replace
        (toReplace: string)
        (replacement: string)
        (string: string)
        : string =
        string.Replace(toReplace, replacement)

    let string_toUpper (string: string) : string = string.ToUpper()

    let string_toLower (string: string) : string = string.ToLower()

    let string_concat (separator: string) (strings: list<string>) : string =
        String.concat "" strings

    let string_padLeft
        (newMinimumLength: float)
        (padding: char)
        (string: string)
        : string =
        string.PadLeft(int newMinimumLength, padding)

    let string_padRight
        (newMinimumLength: float)
        (padding: char)
        (string: string)
        : string =
        string.PadRight(int newMinimumLength, padding)

    let string_toInt (string: string) : option<float> =
        let (success, num) = System.Int64.TryParse string

        if success then Some(float num) else None

    let string_toFloat (string: string) : option<float> =
        let (success, num) = System.Double.TryParse string

        if success then Some(num) else None

    let string_slice
        (startIndexInclusive: float)
        (endIndexExclusive: float)
        (string: string)
        : string =
        let realStartIndex =
            if System.Double.IsNegative(startIndexInclusive) then
                String.length string + int startIndexInclusive

            else
                int startIndexInclusive

        let realEndIndexExclusive =
            if System.Double.IsNegative(endIndexExclusive) then
                String.length string + int endIndexExclusive

            else
                int endIndexExclusive

        string.Substring(
            realStartIndex,
            realEndIndexExclusive - 1 - realStartIndex
        )

    let list_member (needle: 'a) (list: list<'a>) : bool =
        List.exists (fun element -> element = needle) list

    let list_product (list: list<float>) : float =
        List.fold basics_mul 1 list

    let list_cons (newHead: 'a) (tail: list<'a>) : list<'a> =
        newHead :: tail
    
    let list_drop (skippedElementCount: float) (list: list<'a>): list<'a> =
        List.skip (int skippedElementCount) list
    
    let list_take (takenElementCount: float) (list: list<'a>): list<'a> =
        List.take (int takenElementCount) list

    let list_sortWith
        (elementCompare: 'a -> 'a -> Basics_Order)
        (list: List<'a>)
        : List<'a> =
        List.sortWith
            (fun a b ->
                match elementCompare a b with
                | Basics_LT -> -1
                | Basics_EQ -> 0
                | Basics_GT -> 1)
            list

    let list_intersperse (sep: 'a) (list: list<'a>) =
        match list with
        | [] -> []
        | listHead :: listTail ->
            List.foldBack
                (fun x soFar -> x :: sep :: soFar)
                listTail
                [ listHead ]

    let list_foldl
        (reduce: 'a -> 'state -> 'state)
        (initialState: 'state)
        (list: list<'a>)
        : 'state =
        List.fold
            (fun soFar element -> reduce element soFar)
            initialState
            list

    let list_foldr
        (reduce: 'a -> 'state -> 'state)
        (initialState: 'state)
        (list: list<'a>)
        : 'state =
        List.foldBack reduce list initialState

    let list_range (startFloat: float) (endFloat: float) : list<float> =
        [ startFloat..endFloat ]

    let dict_singleton (key: 'key) (value: 'value) : Map<'key, 'value> =
        Map [ (key, value) ]

    let dict_foldr
        (reduce: 'key -> 'value -> 'state -> 'state)
        (initialState: 'state)
        (dict: Map<'key, 'value>)
        =
        Map.foldBack reduce dict initialState

    let dict_foldl
        (reduce: 'key -> 'value -> 'state -> 'state)
        (initialState: 'state)
        (dict: Map<'key, 'value>)
        =
        Map.fold (fun soFar k v -> reduce k v soFar) initialState dict


    let dict_keys (dict: Map<'key, 'value>) : List<'key> =
        Seq.toList (Map.keys dict)

    let dict_values (dict: Map<'key, 'value>) : List<'value> =
        Seq.toList (Map.values dict)

    let dict_diff
        (baseDict: Map<'key, 'a>)
        (dictWithKeysToRemove: Map<'key, 'b>)
        : Map<'key, 'a> =
        Map.fold
            (fun soFar k _ -> Map.remove k soFar)
            baseDict
            dictWithKeysToRemove

    let dict_union
        (aDict: Map<'key, 'a>)
        (bDict: Map<'key, 'a>)
        : Map<'key, 'a> =
        Map.fold (fun soFar k v -> Map.add k v soFar) bDict aDict


    type Parser_Problem =
        | Parser_Expecting of string
        | Parser_ExpectingInt
        | Parser_ExpectingHex
        | Parser_ExpectingOctal
        | Parser_ExpectingBinary
        | Parser_ExpectingFloat
        | Parser_ExpectingNumber
        | Parser_ExpectingVariable
        | Parser_ExpectingSymbol of string
        | Parser_ExpectingKeyword of string
        | Parser_ExpectingEnd
        | Parser_UnexpectedChar
        | Parser_Problem of string
        | Parser_BadRepeat
"""
        ++ (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (declarations
                            |> Print.listMapAndIntersperseAndFlatten
                                (\declaration ->
                                    declaration
                                        |> printFsharpDeclaration
                                )
                                (Print.linebreak
                                    |> Print.followedBy
                                        Print.linebreakIndented
                                )
                        )
                )
                |> Print.toString
           )
    )
        ++ "\n"


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


printExactlyCurlyOpeningVerticalBarSpace : Print.Print
printExactlyCurlyOpeningVerticalBarSpace =
    Print.exactly "{| "


printExactlyParensOpening : Print
printExactlyParensOpening =
    Print.exactly "("


printExactlyParensClosing : Print
printExactlyParensClosing =
    Print.exactly ")"


printExactlyVerticalBarCurlyClosing : Print
printExactlyVerticalBarCurlyClosing =
    Print.exactly "|}"


printExactlyFunSpace : Print
printExactlyFunSpace =
    Print.exactly "fun "


printExactlyUnderscore : Print
printExactlyUnderscore =
    Print.exactly "_"


printExactlyMatch : Print
printExactlyMatch =
    Print.exactly "match"


printExactlyIf : Print
printExactlyIf =
    Print.exactly "if"


printExactlyElse : Print
printExactlyElse =
    Print.exactly "else"
