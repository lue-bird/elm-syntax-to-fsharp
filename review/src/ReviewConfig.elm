module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import EqualsCaseable
import NoAlways
import NoCatchAllForSpecificRemainingPatterns
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDeprecated
import NoDuplicatePorts
import NoExposingEverything
import NoForbiddenWords
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoPrimitiveTypeAlias
import NoRecordAliasConstructor
import NoRecursiveUpdate
import NoSimpleLetBody
import NoSinglePatternCase
import NoUnnecessaryTrailingUnderscore
import NoUnoptimizedRecursion
import NoUnsafeDivision
import NoUnsafePorts
import NoUnsortedCases
import NoUnsortedTopLevelDeclarations
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import OnlyAllSingleUseTypeVarsEndWith_
import Review.Action
import Review.Documentation.CodeSnippet
import Review.ImportSimple
import Review.OpaqueType
import Review.Pattern.As
import Review.Pattern.Record
import Review.PhantomType
import Review.Rule
import Review.VariantValueCount
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes
import ReviewPipelineStyles.Predicates
import Simplify
import UseCamelCase
import VariablesBetweenCaseOf.AccessInCases


config : List Review.Rule.Rule
config =
    [ -- ## documentation
      -- enable on a per-project basis
      --, Review.Documentation.CodeSnippet.check
      Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.NoMissing.rule
        { document = Docs.NoMissing.onlyExposed
        , from = Docs.NoMissing.exposedModules
        }
    , Docs.UpToDateReadmeLinks.rule

    -- ## simplify/refactor
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUnusedPorts.rule
    , Simplify.rule
        (Simplify.defaults |> Simplify.expectNaN)
    , Review.Action.rule

    -- ## limit
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , Review.VariantValueCount.zeroOrOne
        |> Review.Rule.ignoreErrorsForFiles 
            [ "src/ParserFast.elm", "src/ElmSyntaxParserLenient.elm", "src/Print.elm", "src/Rope.elm", "src/ElmSyntaxPrintDefunctionalized.elm" ]
    , [ ReviewPipelineStyles.rightPizzaPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.that
                (ReviewPipelineStyles.Predicates.haveAnyStepThatIs
                    ReviewPipelineStyles.Predicates.aConfusingNonCommutativeFunction
                )
            |> ReviewPipelineStyles.andCallThem
                "|> pipeline with confusing non-commutative function"
      , ReviewPipelineStyles.parentheticalApplicationPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.that
                (ReviewPipelineStyles.Predicates.haveAnyStepThatIs
                    ReviewPipelineStyles.Predicates.aConfusingNonCommutativePrefixOperator
                )
            |> ReviewPipelineStyles.andCallThem
                "parenthetical application with confusing non-commutative prefix operator"
      , ReviewPipelineStyles.leftPizzaPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.andTryToFixThemBy
                ReviewPipelineStyles.Fixes.convertingToParentheticalApplication
            |> ReviewPipelineStyles.andReportCustomError
                "<| pipeline"
                [ "Forbidding `f <| a s` for reasons of simplicity, consistency:"
                , "  - Pipe data before the function: `food |> op ...`"
                , "  - Feed arguments after the function: `... |> opWith (a ...) (b ...)`"
                , "Use the application style `f (a s)` instead"
                ]
      , ReviewPipelineStyles.rightCompositionPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.andReportCustomError
                ">> pipeline"
                [ "Avoid `g >> f` for easier to understand, more consistent code:"
                , [ "Establish a subject: `List.map (\\user -> user |> User.badgeAdd ... |> User.levelIncrease)`"
                  , " for easier readability and scalability (maybe even creating a separate function)"
                  , " when chaining multiple operations"
                  ]
                    |> String.concat
                ]
      , ReviewPipelineStyles.leftCompositionPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.andReportCustomError
                "<< pipeline"
                [ "Avoid `g << f` for easier to understand, more consistent code:"
                , "  - Keep the order data comes from before and gets piped through functions after: `... |> opF |> opG`"
                , [ "Establish a subject: `List.map (\\user -> user |> User.badgeAdd ... |> User.levelIncrease)`"
                  , " for easier readability and scalability (maybe even creating a separate function)"
                  , " when chaining multiple operations"
                  ]
                    |> String.concat
                ]
      ]
        |> ReviewPipelineStyles.rule
    , UseCamelCase.rule
        (UseCamelCase.default
            |> UseCamelCase.withCamel toCamelCase
            |> UseCamelCase.withPascal toCamelCase
        )
    , NoPrimitiveTypeAlias.rule
    , Review.ImportSimple.rule
    , OnlyAllSingleUseTypeVarsEndWith_.rule
    , NoRecordAliasConstructor.rule
    , NoExposingEverything.rule
    , NoForbiddenWords.rule [ "REPLACEME", "TODO", "- []" ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoAlways.rule
    , NoDebug.Log.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoDebug.TodoOrToString.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
    , VariablesBetweenCaseOf.AccessInCases.forbid
        |> Review.Rule.ignoreErrorsForFiles 
            [ "src/ElmSyntaxToFsharp.elm", "src/Print.elm" ]
    , EqualsCaseable.forbid EqualsCaseable.Everywhere
    , NoDeprecated.rule NoDeprecated.defaults
    , NoPrematureLetComputation.rule
    , NoDuplicatePorts.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoRecursiveUpdate.rule
    , NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO"
        |> NoUnoptimizedRecursion.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoSimpleLetBody.rule
    , NoUnnecessaryTrailingUnderscore.rule
    , NoUnsafeDivision.rule
    , Review.Pattern.Record.forbid
    , Review.Pattern.As.forbid
        |> Review.Rule.ignoreErrorsForFiles 
            [ "src/ParserFast.elm", "src/ElmSyntaxParserLenient.elm", "src/Print.elm", "src/Rope.elm", "src/ElmSyntaxPrintDefunctionalized.elm" ]
    , Review.PhantomType.forbid
    , Review.OpaqueType.forbid
    , NoCatchAllForSpecificRemainingPatterns.rule
        |> Review.Rule.ignoreErrorsForFiles 
            [ "src/ParserFast.elm", "src/ElmSyntaxParserLenient.elm", "src/Print.elm", "src/Rope.elm", "src/ElmSyntaxPrintDefunctionalized.elm" ]
    ]
        |> List.map (Review.Rule.ignoreErrorsForDirectories [ "tests/VerifyExamples/" ])


toCamelCase : String -> String
toCamelCase =
    \name ->
        let
            camelFolded : { camelized : String, upperCaseNextRequired : Maybe { underscores : Int } }
            camelFolded =
                name
                    |> String.foldl
                        (\char soFar ->
                            if char |> Char.isUpper then
                                { upperCaseNextRequired = Nothing
                                , camelized = soFar.camelized ++ (char |> String.fromChar)
                                }

                            else if char |> Char.isLower then
                                { upperCaseNextRequired = Nothing
                                , camelized =
                                    case soFar.upperCaseNextRequired of
                                        Just _ ->
                                            soFar.camelized ++ (char |> Char.toUpper |> String.fromChar)

                                        Nothing ->
                                            soFar.camelized ++ (char |> String.fromChar)
                                }

                            else
                                case char of
                                    '_' ->
                                        { upperCaseNextRequired =
                                            Just
                                                { underscores =
                                                    case soFar.upperCaseNextRequired of
                                                        Nothing ->
                                                            1

                                                        Just trail ->
                                                            trail.underscores + 1
                                                }
                                        , camelized = soFar.camelized
                                        }

                                    nonLetterNonUnderscoreChar ->
                                        { upperCaseNextRequired = Nothing
                                        , camelized = soFar.camelized ++ (nonLetterNonUnderscoreChar |> String.fromChar)
                                        }
                        )
                        { camelized = ""
                        , upperCaseNextRequired = Nothing
                        }
        in
        case camelFolded.upperCaseNextRequired of
            Nothing ->
                camelFolded.camelized

            Just trail ->
                camelFolded.camelized ++ String.repeat trail.underscores "_"
