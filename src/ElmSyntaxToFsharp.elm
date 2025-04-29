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
    | FsharpPatternRecordInexhaustive (FastSet.Set String)
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
                            |> List.map variableNameDisambiguateFromFsharpKeywords
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
    Print.exactly
        ("type "
            ++ generatedFsharpRecordTypeAliasName fsharpRecordFields
            ++ (fsharpRecordFields
                    |> fsharpTypeParametersToString
               )
            ++ " ="
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
                                (Print.exactly ((fieldName |> stringFirstCharToUpper) ++ ":")
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


fsharpTypeFloat : FsharpType
fsharpTypeFloat =
    FsharpTypeConstruct
        { moduleOrigin = Nothing
        , name = "float"
        , arguments = []
        }


fsharpTypeInt : FsharpType
fsharpTypeInt =
    FsharpTypeConstruct
        { moduleOrigin = Nothing
        , name = "int"
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
                Ok fsharpTypeInt

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
                                                    |> stringFirstCharToUpper
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
                                            ( fieldName |> variableNameDisambiguateFromFsharpKeywords
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

                ElmSyntaxTypeInfer.TypeRecordExtension _ ->
                    Err "extensible record types are not supported"


typeExpandFunctionOutputReverse :
    ElmSyntaxTypeInfer.Type String
    -> List (ElmSyntaxTypeInfer.Type String)
typeExpandFunctionOutputReverse typeNode =
    typeExpandFunctionOutputIntoReverse [] typeNode


typeExpandFunctionOutputIntoReverse :
    List (ElmSyntaxTypeInfer.Type String)
    -> ElmSyntaxTypeInfer.Type String
    -> List (ElmSyntaxTypeInfer.Type String)
typeExpandFunctionOutputIntoReverse soFarReverse syntaxType =
    case syntaxType of
        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
            typeExpandFunctionOutputIntoReverse
                (typeFunction.input :: soFarReverse)
                typeFunction.output

        otherType ->
            otherType :: soFarReverse


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
                                                    |> stringFirstCharToUpper
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
                                    ( name |> variableNameDisambiguateFromFsharpKeywords
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


typeAnnotationExpandFunctionOutputReverse :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeAnnotationExpandFunctionOutputReverse typeNode =
    typeAnnotationExpandFunctionOutputIntoReverse [] typeNode


typeAnnotationExpandFunctionOutputIntoReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeAnnotationExpandFunctionOutputIntoReverse soFarReverse (Elm.Syntax.Node.Node fullRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            typeAnnotationExpandFunctionOutputIntoReverse
                (inputNode :: soFarReverse)
                outputNode

        otherType ->
            Elm.Syntax.Node.Node fullRange otherType :: soFarReverse


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
                        Print.withIndentIncreasedBy 3
                            outputPartPrint
                    )
           )
        |> Print.listIntersperseAndFlatten
            (Print.spaceOrLinebreakIndented fullLineSpread
                |> Print.followedBy
                    (Print.exactly "-> ")
            )


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

        FsharpTypeVariable variable ->
            FsharpTypeVariable variable :: soFar


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
    Basics.max (-2 ^ 31) (Basics.min (2 ^ 31 - 1) int)
        |> String.fromInt


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
                "\\u{" ++ characterHex otherCharacter ++ "}"

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
                "\\u{" ++ characterHex otherCharacter ++ "}"

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
                fieldNames : FastSet.Set String
                fieldNames =
                    fields
                        |> listMapAndToFastSet
                            (\fieldTypedNode ->
                                fieldTypedNode.value |> variableNameDisambiguateFromFsharpKeywords
                            )
            in
            Ok
                { pattern = FsharpPatternRecordInexhaustive fieldNames
                , introducedVariables = fieldNames
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
                                            |> stringFirstCharToUpper
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


patternConsExpand :
    ElmSyntaxTypeInfer.TypedNode
        (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
        (ElmSyntaxTypeInfer.Type String)
    ->
        { initialElements :
            List
                (ElmSyntaxTypeInfer.TypedNode
                    (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
                    (ElmSyntaxTypeInfer.Type String)
                )
        , tail :
            ElmSyntaxTypeInfer.TypedNode
                (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
                (ElmSyntaxTypeInfer.Type String)
        }
patternConsExpand patternNode =
    patternConsExpandFromInitialElementsReverse [] patternNode


patternConsExpandFromInitialElementsReverse :
    List
        (ElmSyntaxTypeInfer.TypedNode
            (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
            (ElmSyntaxTypeInfer.Type String)
        )
    ->
        ElmSyntaxTypeInfer.TypedNode
            (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
            (ElmSyntaxTypeInfer.Type String)
    ->
        { initialElements :
            List
                (ElmSyntaxTypeInfer.TypedNode
                    (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
                    (ElmSyntaxTypeInfer.Type String)
                )
        , tail :
            ElmSyntaxTypeInfer.TypedNode
                (ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type String))
                (ElmSyntaxTypeInfer.Type String)
        }
patternConsExpandFromInitialElementsReverse initialElementsSoFarReverse syntaxPattern =
    let
        wrapInTypedNode value =
            { value = value
            , type_ = syntaxPattern.type_
            , range = syntaxPattern.range
            }
    in
    case syntaxPattern.value of
        ElmSyntaxTypeInfer.PatternListCons listCons ->
            patternConsExpandFromInitialElementsReverse
                (listCons.head :: initialElementsSoFarReverse)
                listCons.tail

        ElmSyntaxTypeInfer.PatternParenthesized inParens ->
            patternConsExpandFromInitialElementsReverse initialElementsSoFarReverse
                inParens

        ElmSyntaxTypeInfer.PatternIgnored ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternIgnored |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternUnit ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternUnit |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternChar char ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternChar char |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternString string ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternString string |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternInt int ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternInt int |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternTuple parts ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternTuple parts |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternTriple parts ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternTriple parts |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternRecord fields ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternRecord fields |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternListExact elements ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternListExact elements |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternVariable variableName ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternVariable variableName |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternVariant variant ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternVariant variant |> wrapInTypedNode
            }

        ElmSyntaxTypeInfer.PatternAs patternAs ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = ElmSyntaxTypeInfer.PatternAs patternAs |> wrapInTypedNode
            }


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
                    Just { moduleOrigin = Nothing, name = "int" }

                "Float" ->
                    Just { moduleOrigin = Nothing, name = "float" }

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
                    Just { moduleOrigin = Nothing, name = "list" }

                _ ->
                    Nothing

        [ "Dict" ] ->
            case reference.name of
                "Dict" ->
                    Just { moduleOrigin = Nothing, name = "Map" }

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                "Problem" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Problem" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Maybe" ->
                    Just { moduleOrigin = Nothing, name = "option" }

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
                    Just { moduleOrigin = Nothing, name = "basicsAlways" }

                "compare" ->
                    Just { moduleOrigin = Nothing, name = "basics_compare" }

                "max" ->
                    Just { moduleOrigin = Nothing, name = "max" }

                "min" ->
                    Just { moduleOrigin = Nothing, name = "min" }

                "LT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_LT" }

                "EQ" ->
                    Just { moduleOrigin = Nothing, name = "Basics_EQ" }

                "GT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_GT" }

                "True" ->
                    Just { moduleOrigin = Nothing, name = "true" }

                "False" ->
                    Just { moduleOrigin = Nothing, name = "false" }

                "not" ->
                    Just { moduleOrigin = Nothing, name = "not" }

                "xor" ->
                    Just { moduleOrigin = Nothing, name = "basics_neq" }

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
                    case reference.type_ of
                        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                            case typeFunction.input of
                                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct inputTypeConstruct) ->
                                    case ( inputTypeConstruct.moduleOrigin, inputTypeConstruct.name ) of
                                        ( [ "Basics" ], "Float" ) ->
                                            Just { moduleOrigin = Nothing, name = "basics_fnegate" }

                                        _ ->
                                            -- assume Int
                                            Just { moduleOrigin = Nothing, name = "basics_inegate" }

                                _ ->
                                    -- assume Int
                                    Just { moduleOrigin = Nothing, name = "basics_inegate" }

                        _ ->
                            -- assume Int
                            Just { moduleOrigin = Nothing, name = "basics_inegate" }

                "abs" ->
                    case reference.type_ of
                        ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeFunction typeFunction) ->
                            case typeFunction.input of
                                ElmSyntaxTypeInfer.TypeNotVariable (ElmSyntaxTypeInfer.TypeConstruct inputTypeConstruct) ->
                                    case ( inputTypeConstruct.moduleOrigin, inputTypeConstruct.name ) of
                                        ( [ "Basics" ], "Float" ) ->
                                            Just { moduleOrigin = Nothing, name = "basics_fabs" }

                                        _ ->
                                            -- assume Int
                                            Just { moduleOrigin = Nothing, name = "basics_iabs" }

                                _ ->
                                    -- assume Int
                                    Just { moduleOrigin = Nothing, name = "basics_iabs" }

                        _ ->
                            -- assume Int
                            Just { moduleOrigin = Nothing, name = "basics_iabs" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "float" }

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
                "isEmpty" ->
                    Just { moduleOrigin = Nothing, name = "string_isEmpty" }

                "length" ->
                    Just { moduleOrigin = Nothing, name = "string_length" }

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
                    Just { moduleOrigin = Nothing, name = "string_join" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "string_filter" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "string_any" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "string_all" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "string_map" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "string_repeat" }

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
                    Just { moduleOrigin = Nothing, name = "string_fromInt" }

                "fromFloat" ->
                    Just { moduleOrigin = Nothing, name = "string_fromFloat" }

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
                "toCode" ->
                    Just { moduleOrigin = Nothing, name = "int" }

                "fromCode" ->
                    Just { moduleOrigin = Nothing, name = "char" }

                "toLower" ->
                    Just { moduleOrigin = Just "System.Char", name = "toLower" }

                "toUpper" ->
                    Just { moduleOrigin = Just "System.Char", name = "toUpper" }

                "isHexDigit" ->
                    Just { moduleOrigin = Nothing, name = "char_isHexDigit" }

                "isDigit" ->
                    Just { moduleOrigin = Nothing, name = "char_isDigit" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "singleton" ->
                    Just { moduleOrigin = Just "List", name = "exactlyOne" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "List", name = "isEmpty" }

                "length" ->
                    Just { moduleOrigin = Just "List", name = "length" }

                "member" ->
                    Just { moduleOrigin = Nothing, name = "list_member" }

                "minimum" ->
                    Just { moduleOrigin = Nothing, name = "list_minimum" }

                "maximum" ->
                    Just { moduleOrigin = Nothing, name = "list_maximum" }

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
                                                            Just { moduleOrigin = Nothing, name = "list_fproduct" }

                                                        _ ->
                                                            -- assume List Int
                                                            Just { moduleOrigin = Nothing, name = "list_iproduct" }

                                                _ ->
                                                    -- assume List Int
                                                    Just { moduleOrigin = Nothing, name = "list_iproduct" }

                                        _ ->
                                            -- assume List Int
                                            Just { moduleOrigin = Nothing, name = "list_iproduct" }

                                _ ->
                                    -- assume List Int
                                    Just { moduleOrigin = Nothing, name = "list_iproduct" }

                        _ ->
                            -- assume List Int
                            Just { moduleOrigin = Nothing, name = "list_iproduct" }

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
                    Just { moduleOrigin = Just "List", name = "take" }

                "drop" ->
                    Just { moduleOrigin = Just "List", name = "skip" }

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
    (reference.moduleOrigin
        |> String.concat
        |> stringFirstCharToLower
    )
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
                        |> FastSet.toList
                        |> Print.listMapAndIntersperseAndFlatten
                            (\fieldName ->
                                Print.exactly
                                    ((fieldName |> stringFirstCharToUpper)
                                        ++ " = "
                                        ++ fieldName
                                    )
                            )
                            (Print.exactly "; ")
                    )
                |> Print.followedBy (Print.exactly " }")

        FsharpPatternVariant patternVariant ->
            Print.exactly patternVariant.name
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
                                (Print.exactly ((fieldName |> stringFirstCharToUpper) ++ " =")
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
across multiple modules to value, function and [`FsharpTypeDeclaration`](#FsharpTypeDeclaration)s.
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
                    { errors = [], types = ElmSyntaxTypeInfer.elmCoreTypes }

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
                                                    -- this is pretty shaky but works for now
                                                    let
                                                        name : String
                                                        name =
                                                            syntaxValueOrFunctionDeclaration.declaration
                                                                |> Elm.Syntax.Node.value
                                                                |> .name
                                                                |> Elm.Syntax.Node.value
                                                    in
                                                    if
                                                        (name |> String.contains "encode")
                                                            || (name |> String.contains "fromList")
                                                            || (name
                                                                    |> String.toLower
                                                                    |> String.contains "decode"
                                                               )
                                                    then
                                                        Nothing

                                                    else
                                                        Just syntaxValueOrFunctionDeclaration

                                                _ ->
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
                                                                                |> stringFirstCharToUpper
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
                                                                                        |> stringFirstCharToUpper
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
                                                                                                                |> stringFirstCharToUpper
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
    ElmSyntaxTypeInfer.Pattern (ElmSyntaxTypeInfer.Type comparableTypeVariable)
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
                        , variantLookup =
                            moduleContext.variantLookup
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
                                                |> variableNameDisambiguateFromFsharpKeywords
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
                                                            , name = "string_append"
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
                                                    |> stringFirstCharToUpper
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
                                                -- TODO should be redundant because variant check
                                                if reference.name |> stringFirstCharIsUpper then
                                                    referenceToFsharpName
                                                        { moduleOrigin = reference.moduleOrigin
                                                        , name = reference.name
                                                        }
                                                        |> stringFirstCharToUpper

                                                else
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
                                                { moduleOrigin = Nothing, name = "basics_fnegate" }

                                            _ ->
                                                -- assume Int
                                                { moduleOrigin = Nothing, name = "basics_inegate" }

                                    _ ->
                                        -- assume Int
                                        { moduleOrigin = Nothing, name = "basics_inegate" }
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
                                |> variableNameDisambiguateFromFsharpKeywords
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
                                        |> variableNameDisambiguateFromFsharpKeywords
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
                    if (lambdaParameter1Up |> List.length) == 1 + (call.argument1Up |> List.length) then
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
                        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                        , variantLookup =
                            context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            FastSet.union
                                (parameters
                                    |> listMapToFastSetsAndUnify .introducedVariables
                                )
                                context.variablesFromWithinDeclarationInScope
                        }
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
                                    Ok { moduleOrigin = Nothing, name = "basics_fpow" }

                                _ ->
                                    -- assume Int
                                    Ok { moduleOrigin = Nothing, name = "basics_ipow" }

                        _ ->
                            -- assume Int
                            Ok { moduleOrigin = Nothing, name = "basics_ipow" }

                _ ->
                    -- assume Int
                    Ok { moduleOrigin = Nothing, name = "basics_ipow" }

        "==" ->
            Ok { moduleOrigin = Nothing, name = "basics_eq" }

        "/=" ->
            Ok { moduleOrigin = Nothing, name = "basics_neq" }

        "||" ->
            Ok { moduleOrigin = Nothing, name = "basics_or" }

        "&&" ->
            Ok { moduleOrigin = Nothing, name = "basics_and" }

        "<" ->
            Ok { moduleOrigin = Nothing, name = "(<)" }

        ">" ->
            Ok { moduleOrigin = Nothing, name = "(>)" }

        "<=" ->
            Ok { moduleOrigin = Nothing, name = "(<=)" }

        ">=" ->
            Ok { moduleOrigin = Nothing, name = "(>=)" }

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
                                        printFsharpTypeParenthesizedIfSpaceSeparated
                                            parameter.type_
                                in
                                Print.exactly "("
                                    |> Print.followedBy
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
                                    |> Print.followedBy
                                        (Print.exactly ")")
                            )

                parametersLineSpread : Print.LineSpread
                parametersLineSpread =
                    parameterPrints
                        |> Print.lineSpreadListMapAndCombine
                            Print.lineSpread
            in
            Print.exactly
                (fsharpValueOrFunctionDeclaration.name
                    ++ (fsharpValueOrFunctionDeclaration.type_
                            |> fsharpTypeContainedVariables
                            |> FastSet.toList
                            |> fsharpTypeParametersToString
                       )
                )
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 4
                        (parameterPrints
                            |> Print.listMapAndIntersperseAndFlatten
                                (\parameterPrint -> parameterPrint)
                                (Print.spaceOrLinebreakIndented parametersLineSpread)
                        )
                    )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.exactly " ="
                            |> Print.followedBy
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (printFsharpExpressionParenthesizedIfWithLetDeclarations
                                            function.result
                                        )
                                )
                        )
                    )

        resultNotFunction ->
            Print.exactly
                (fsharpValueOrFunctionDeclaration.name
                    ++ (fsharpValueOrFunctionDeclaration.type_
                            |> fsharpTypeContainedVariables
                            |> FastSet.toList
                            |> fsharpTypeParametersToString
                       )
                )
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
                                        printFsharpTypeParenthesizedIfSpaceSeparated
                                            parameter.type_
                                in
                                Print.exactly "("
                                    |> Print.followedBy
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
                                    |> Print.followedBy
                                        (Print.exactly ")")
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
                                (\parameterPrint -> parameterPrint)
                                (Print.spaceOrLinebreakIndented parametersLineSpread)
                        )
                    )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.exactly " ="
                            |> Print.followedBy
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (printFsharpExpressionParenthesizedIfWithLetDeclarations
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
                        ("." ++ (syntaxRecordAccess.field |> stringFirstCharToUpper))
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
    Print.exactly "( "
        |> Print.followedBy
            ((part0Print :: part1Print :: part2UpPrints)
                |> Print.listMapAndIntersperseAndFlatten
                    (\partPrint ->
                        Print.withIndentIncreasedBy 2 partPrint
                    )
                    (Print.spaceOrLinebreakIndented lineSpread
                        |> Print.followedBy
                            (Print.exactly ", ")
                    )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy (Print.exactly ")")


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
                                        (Print.exactly ((fieldName |> stringFirstCharToUpper) ++ " ="))
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
                                printFsharpTypeParenthesizedIfSpaceSeparated
                                    parameter.type_
                        in
                        Print.exactly "("
                            |> Print.followedBy
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
                            |> Print.followedBy
                                (Print.exactly ")")
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
                        (printFsharpExpressionParenthesizedIfSpaceSeparated
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
    printParenthesized
        { opening = "("
        , closing = ")"
        , inner =
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
                                Print.exactly "let "
                                    |> -- I would have thought let rec
                                       -- but the compiler disagrees
                                       Print.followedBy
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
        }


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
    printParenthesized
        { opening = "("
        , closing = ")"
        , inner =
            letDestructuring.pattern
                |> printFsharpPatternNotParenthesized
                |> Print.followedBy
                    (Print.exactly ": ")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4 patternTypePrint)
        }
        |> Print.followedBy (Print.exactly " =")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionNotParenthesized letDestructuring.expression)
                )
            )


printFsharpExpressionMatchCase :
    { pattern : FsharpPattern
    , patternType : FsharpType
    , result : FsharpExpression
    }
    -> Print
printFsharpExpressionMatchCase branch =
    let
        patternPrint : Print
        patternPrint =
            printFsharpPatternNotParenthesized branch.pattern

        patternTypePrint : Print
        patternTypePrint =
            printFsharpTypeParenthesizedIfSpaceSeparated
                branch.patternType
    in
    printParenthesized
        { opening = "("
        , closing = ")"
        , inner =
            Print.withIndentIncreasedBy 2
                patternPrint

        --|> Print.followedBy (Print.exactly ":")
        --|> Print.followedBy
        --    (Print.withIndentAtNextMultipleOf4
        --        (Print.spaceOrLinebreakIndented
        --            ((patternPrint |> Print.lineSpread)
        --                |> Print.lineSpreadMergeWith
        --                    (\() -> patternTypePrint |> Print.lineSpread)
        --            )
        --            |> Print.followedBy
        --                patternTypePrint
        --        )
        --    )
        }
        |> Print.followedBy (Print.exactly " =>")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printFsharpExpressionParenthesizedIfSpaceSeparated
                            branch.result
                        )
                )
            )
        |> Print.followedBy (Print.exactly ",")


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
                                            Print.exactly "type "
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
    let inline basics_always (result: 'result) (_: '_ignored) : 'result = result

    let inline basics_eq (a: 'a) (b: 'a) = a = b
    let inline basics_neq (a: 'a) (b: 'a) = a <> b
    let inline basics_flt (a: float) (b: float) : bool = a < b
    let inline basics_ilt (a: int) (b: int) : bool = a < b
    let inline basics_fle (a: float) (b: float) : bool = a <= b
    let inline basics_ile (a: int) (b: int) : bool = a <= b
    let inline basics_fgt (a: float) (b: float) : bool = a > b
    let inline basics_igt (a: int) (b: int) : bool = a > b
    let inline basics_fge (a: float) (b: float) : bool = a >= b
    let inline basics_ige (a: int) (b: int) : bool = a >= b

    type Basics_Order =
        | Basics_LT
        | Basics_EQ
        | Basics_GT

    let inline basics_compare (a: 'a) (b: 'a) : Basics_Order =
        let comparisonMagnitude = compare a b

        if comparisonMagnitude = 0 then Basics_EQ
        else if comparisonMagnitude < 0 then Basics_LT
        else Basics_GT

    let inline basics_fabs (n: float) : float = System.Double.Abs(n)
    let inline basics_iabs (n: int) : int = System.Int32.Abs(n)
    let inline basics_fnegate (n: float) : float = -n
    let inline basics_inegate (n: int) : int = -n
    let inline basics_fadd (a: float) (b: float) : float = a + b
    let inline basics_iadd (a: int) (b: int) : int = a + b
    let inline basics_fsub (a: float) (b: float) : float = a - b
    let inline basics_isub (a: int) (b: int) : int = a - b
    let inline basics_fmul (a: float) (b: float) : float = a * b
    let inline basics_imul (a: int) (b: int) : int = a * b
    let inline basics_fdiv (a: float) (b: float) : float = a / b
    let inline basics_idiv (a: int) (b: int) : int = a / b
    let inline basics_remainderBy (divisor: int) (toDivide: int) : int =
        toDivide % divisor

    let basics_modBy (divisor: int) (toDivide: int) : int =
        let remainder = toDivide % divisor

        if
            (remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0)
        then
            remainder + toDivide


        else
            remainder

    let inline basics_fpow (a: float) (b: float) : float = a ** b
    let inline basics_ipow (a: int) (b: int) : int = int (float a ** float b)

    let inline basics_and (a: bool) (b: bool) : bool = a && b
    let inline basics_or (a: bool) (b: bool) : bool = a || b

    let inline char_isHexDigit (ch : char) : bool =
        System.Char.IsAsciiHexDigit(ch)
    let inline char_isDigit (ch : char) : bool =
        System.Char.IsAsciiDigit(ch)
    
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

    let rec string_isEmpty (stringToCheck: StringRope) : bool =
        match stringToCheck with
        | StringRopeOne string -> String.IsNullOrEmpty(string)
        | StringRopeAppend (left, right) ->
            string_isEmpty left && string_isEmpty right

    let string_length (str: StringRope) : int =
        String.length (StringRope.toString str)
    
    let string_repeat (repetitions: int) (segment: StringRope) : StringRope =
        StringRopeOne (String.replicate repetitions (StringRope.toString segment))

    let string_toList (string: StringRope) : list<char> =
        List.ofArray ((StringRope.toString string).ToCharArray())

    let string_fromList (chars: list<char>) : StringRope =
        StringRopeOne (new string (List.toArray chars))

    let string_contains (substringRope: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).Contains(StringRope.toString substringRope)

    let string_startsWith (start: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).StartsWith(StringRope.toString start)

    let string_endsWith (ending: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).EndsWith(StringRope.toString ending)

    let string_any
        (charIsNeedle: char -> bool)
        (string: StringRope)
        : bool =
        // can be optimized
        String.exists charIsNeedle (StringRope.toString string)

    let string_all
        (charIsExpected: char -> bool)
        (string: StringRope)
        : bool =
        // can be optimized
        String.forall charIsExpected (StringRope.toString string)

    let string_map
        (charChange: char -> char)
        (string: StringRope)
        : StringRope =
        // can be optimized
        StringRopeOne
            (String.map charChange (StringRope.toString string))

    let string_filter
        (charShouldBeKept: char -> bool)
        (string: StringRope)
        : StringRope =
        // can be optimized
        StringRopeOne
            (String.filter charShouldBeKept (StringRope.toString string))

    let string_foldl
        (reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: StringRope)
        : 'folded =
        // can be optimized
        Array.fold (fun soFar char -> reduce char soFar) initialFolded
            ((StringRope.toString string).ToCharArray())

    let string_foldr
        (reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: StringRope)
        : 'folded =
        // can be optimized
        Array.foldBack reduce
            ((StringRope.toString string).ToCharArray())
            initialFolded

    let string_trim (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).Trim())
    let string_trimLeft (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).TrimStart())
    let string_trimRight (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).TrimEnd())

    let string_right (takenElementCount: int) (stringRope: StringRope): StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                String.length string - takenElementCount,
                takenElementCount
            ))

    let string_left (skippedElementCount: int) (string: StringRope) : StringRope = 
        StringRopeOne
            ((StringRope.toString string).Substring(0, skippedElementCount))
    
    let string_dropRight (skippedElementCount: int) (stringRope: StringRope) : StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                0,
                String.length string - skippedElementCount
            ))

    let string_dropLeft (skippedElementCount: int) (stringRope: StringRope) : StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                skippedElementCount,
                String.length string - skippedElementCount
            ))

    let inline string_append (early: StringRope) (late: StringRope) : StringRope =
        StringRopeAppend (early, late)
    let string_fromChar (char: char) : StringRope = StringRopeOne (string char)

    let string_cons (newHeadChar: char) (late: StringRope) : StringRope =
        StringRopeAppend (StringRopeOne (string newHeadChar), late)

    let string_split (separator: StringRope) (string: StringRope) : list<StringRope> =
        List.ofArray
            (Array.map (fun segment -> StringRopeOne segment)
                ((StringRope.toString string).Split(StringRope.toString separator))
            )

    let string_lines (string: StringRope) : list<StringRope> =
        List.ofArray (
            (Array.map (fun line -> StringRopeOne line)
                ((StringRope.toString string)
                    .Replace("\\r\\n", "\\n")
                    .Split("\\n")
                )
            )
        )

    let string_reverse (string: StringRope) : StringRope =
        StringRopeOne
            (new string (Array.rev ((StringRope.toString string).ToCharArray())))

    let string_replace
        (toReplace: StringRope)
        (replacement: StringRope)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).Replace(
                StringRope.toString toReplace,
                StringRope.toString replacement
            ))

    let string_toUpper (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).ToUpper())
    let string_toLower (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).ToLower())

    let string_join (separator: StringRope) (strings: list<StringRope>) : StringRope =
        StringRopeOne
            (String.concat
                (StringRope.toString separator)
                (List.map StringRope.toString strings)
            )
    let string_concat (strings: list<StringRope>) : StringRope =
        StringRopeOne
            (String.concat "" (List.map StringRope.toString strings))

    let string_padLeft
        (newMinimumLength: int)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).PadLeft(newMinimumLength, padding))

    let string_padRight
        (newMinimumLength: int)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).PadRight(newMinimumLength, padding))
    
    let string_fromFloat (n: float) : StringRope =
        StringRopeOne (string n)
    let string_fromInt (n: int) : StringRope =
        StringRopeOne (string n)

    let string_toInt (string: StringRope) : option<int> =
        let (success, num) = System.Int32.TryParse (StringRope.toString string)

        if success then Some num else None

    let string_toFloat (string: StringRope) : option<float> =
        let (success, num) = System.Double.TryParse (StringRope.toString string)

        if success then Some num else None

    let string_slice
            (startInclusivePossiblyNegative: int)
            (endExclusivePossiblyNegative: int)
            (stringRope: StringRope)
            : StringRope =
        let string = StringRope.toString stringRope
        let realStartIndex: int =
            if (startInclusivePossiblyNegative < 0) then
                max
                    0
                    (startInclusivePossiblyNegative + String.length string)
            else
                startInclusivePossiblyNegative
        let realEndIndexExclusive: int =
            if (endExclusivePossiblyNegative < 0) then
                max
                    0
                    (endExclusivePossiblyNegative + String.length(string))
            else
                min
                    endExclusivePossiblyNegative
                    (String.length string)

        if (realStartIndex >= realEndIndexExclusive) then
            stringRopeEmpty
        else
            StringRopeOne
                (string.Substring(
                    realStartIndex,
                    realEndIndexExclusive - realStartIndex
                ))

    let list_member (needle: 'a) (list: list<'a>) : bool =
        List.exists (fun element -> element = needle) list
    
    let list_minimum (list: List<'a>): option<'a> =
        match list with
        | [] -> None
        | _ :: _ -> Some (List.min list)
    
    let list_maximum (list: List<'a>): option<'a> =
        match list with
        | [] -> None
        | _ :: _ -> Some (List.max list)

    let list_fproduct (list: list<float>) : float =
        List.fold (*) 1.0 list
    let list_iproduct (list: list<int>) : int =
        List.fold (*) 1 list

    let inline list_cons (newHead: 'a) (tail: list<'a>) : list<'a> =
        newHead :: tail

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

    let inline list_range (startFloat: int) (endFloat: int) : list<int> =
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


resultAndThen3 :
    (a -> b -> c -> Result error d)
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
resultAndThen3 abToResult aResult bResult cResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    case cResult of
                        Err error ->
                            Err error

                        Ok c ->
                            abToResult a b c


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
