module ElmSyntaxToFsharpTests exposing (suite)

import Elm.Parser
import ElmSyntaxToFsharp
import Expect
import FastDict
import FastSet
import Test exposing (Test)


suite : Test
suite =
    Test.describe "elm-syntax-to-fsharp"
        [ Test.test ":: with multiple initial elements and final tail variable"
            (\() ->
                """module A exposing (..)
a0 =
    case [] of
        b :: Just c {- 0 -} {- 1 -} :: (d) ->
            b

        _ ->
            Maybe.Nothing

a1 =
    case [] of
        b :: Just c {- 0 -} {- 1 -} :: (_) ->
            b

        _ ->
            Nothing

a2 x =
    case x of
        (({y,z}::tail), Maybe.Nothing as nothing, (Just[ "" ],0)) ->
            0
        _ ->
            1
"""
                    |> expectTranspiledToFsharpStringAs
                        """namespace global

module rec Elm =
    let Basics_always (result: 'result) (_: '_ignored) : 'result = result

    let Basics_eq (a: 'a) (b: 'a) = a = b
    let Basics_neq (a: 'a) (b: 'a) = a <> b
    let Basics_lt (a: float) (b: float) : bool = a < b
    let Basics_le (a: float) (b: float) : bool = a <= b
    let Basics_gt (a: float) (b: float) : bool = a > b
    let Basics_ge (a: float) (b: float) : bool = a >= b

    type Basics_Order =
        | Basics_LT
        | Basics_EQ
        | Basics_GT

    let Basics_compare (a: 'a) (b: 'a) : Basics_Order =
        let comparisonMagnitude = compare a b

        if comparisonMagnitude = 0 then Basics_EQ
        else if comparisonMagnitude < 0 then Basics_LT
        else Basics_GT

    let Basics_negate (float: float) : float = -float

    let Basics_add (a: float) (b: float) : float = a + b

    let Basics_sub (a: float) (b: float) : float = a - b

    let Basics_mul (a: float) (b: float) : float = a * b

    let Basics_fdiv (a: float) (b: float) : float = a / b

    let Basics_idiv (a: float) (b: float) : float = truncate (a / b)

    let Basics_remainderBy (divisor: float) (toDivide: float) : float =
        toDivide % divisor

    let Basics_modBy (divisor: float) (toDivide: float) : float =
        let remainder = toDivide % divisor

        if
            (remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0)
        then
            remainder + toDivide


        else
            remainder

    let Basics_pow (a: float) (b: float) : float = a ** b

    let Basics_and (a: bool) (b: bool) = a && b

    let Basics_or (a: bool) (b: bool) = a || b

    let String_isEmpty (stringToCheck: string) : bool = stringToCheck = ""

    let String_toList (string: string) : list<char> =
        List.ofArray (string.ToCharArray())

    let String_fromList (chars: list<char>) =
        new string (List.toArray chars)

    let String_contains (substring: string) (string: string) : bool =
        string.Contains(substring)

    let String_startsWith (start: string) (string: string) : bool =
        string.StartsWith(start)

    let String_endsWith (ending: string) (string: string) : bool =
        string.EndsWith(ending)

    let String_foldl
        (reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: string)
        : 'folded =
        Array.fold (fun soFar char -> reduce char soFar) initialFolded (string.ToCharArray())

    let String_foldr
        (reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: string)
        : 'folded =
        Array.foldBack reduce (string.ToCharArray()) initialFolded

    let String_trim (string: string) : string = string.Trim()

    let String_trimLeft (string: string) : string = string.TrimStart()

    let String_trimRight (string: string) : string = string.TrimEnd()

    let String_right (takenElementCount: float) (string: string): string = 
        string.Substring(
            String.length string - int takenElementCount - 1,
            int takenElementCount
        )

    let String_left (skippedElementCount: float) (string: string): string = 
        string.Substring(
            0,
            int skippedElementCount
        )
    
    let String_dropRight (skippedElementCount: float) (string: string): string = 
        string.Substring(
            0,
            String.length string - int skippedElementCount
        )

    let String_dropLeft (skippedElementCount: float) (string: string): string = 
        string.Substring(
            int skippedElementCount - 1,
            String.length string - int skippedElementCount
        )

    let String_append (early: string) (late: string) : string = early + late

    let String_fromChar (char: char) : string = string char

    let String_cons (newHeadChar: char) (late: string) : string =
        String_fromChar newHeadChar + late

    let String_split (separator: string) (string: string) : list<string> =
        List.ofArray (string.Split(separator))

    let String_lines (string: string) : list<string> =
        List.ofArray (
            string
                .Replace("\\r\\n", "\\n")
                .Split("\\n")
        )

    let String_reverse (string: string) : string =
        new string (Array.rev (string.ToCharArray()))

    let String_replace
        (toReplace: string)
        (replacement: string)
        (string: string)
        : string =
        string.Replace(toReplace, replacement)

    let String_toUpper (string: string) : string = string.ToUpper()

    let String_toLower (string: string) : string = string.ToLower()

    let String_concat (separator: string) (strings: list<string>) : string =
        String.concat "" strings

    let String_padLeft
        (newMinimumLength: float)
        (padding: char)
        (string: string)
        : string =
        string.PadLeft(int newMinimumLength, padding)

    let String_padRight
        (newMinimumLength: float)
        (padding: char)
        (string: string)
        : string =
        string.PadRight(int newMinimumLength, padding)

    let String_toInt (string: string) : option<float> =
        let (success, num) = System.Int64.TryParse string

        if success then Some(float num) else None

    let String_toFloat (string: string) : option<float> =
        let (success, num) = System.Double.TryParse string

        if success then Some(num) else None

    let String_slice
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

    let List_member (needle: 'a) (list: list<'a>) : bool =
        List.exists (fun element -> element = needle) list

    let List_product (list: list<float>) : float =
        List.fold Basics_mul 1 list

    let List_cons (newHead: 'a) (tail: list<'a>) : list<'a> =
        newHead :: tail
    
    let List_drop (skippedElementCount: float) (list: list<'a>): list<'a> =
        List.skip (int skippedElementCount) list
    
    let List_take (takenElementCount: float) (list: list<'a>): list<'a> =
        List.take (int takenElementCount) list

    let List_sortWith
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

    let List_intersperse (sep: 'a) (list: list<'a>) =
        match list with
        | [] -> []
        | listHead :: listTail ->
            List.foldBack
                (fun x soFar -> x :: sep :: soFar)
                listTail
                [ listHead ]

    let List_foldl
        (reduce: 'a -> 'state -> 'state)
        (initialState: 'state)
        (list: list<'a>)
        : 'state =
        List.fold
            (fun soFar element -> reduce element soFar)
            initialState
            list

    let List_foldr
        (reduce: 'a -> 'state -> 'state)
        (initialState: 'state)
        (list: list<'a>)
        : 'state =
        List.foldBack reduce list initialState

    let List_range (startFloat: float) (endFloat: float) : list<float> =
        [ startFloat..endFloat ]

    let Dict_singleton (key: 'key) (value: 'value) : Map<'key, 'value> =
        Map [ (key, value) ]

    let Dict_foldr
        (reduce: 'key -> 'value -> 'state -> 'state)
        (initialState: 'state)
        (dict: Map<'key, 'value>)
        =
        Map.foldBack reduce dict initialState

    let Dict_foldl
        (reduce: 'key -> 'value -> 'state -> 'state)
        (initialState: 'state)
        (dict: Map<'key, 'value>)
        =
        Map.fold (fun soFar k v -> reduce k v soFar) initialState dict


    let Dict_keys (dict: Map<'key, 'value>) : List<'key> =
        Seq.toList (Map.keys dict)

    let Dict_values (dict: Map<'key, 'value>) : List<'value> =
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

    let a_a0 =
        match [] with
        | b :: ((Some c) :: d) ->
            b

        | _ ->
            None

    let a_a1 =
        match [] with
        | b :: ((Some c) :: _) ->
            b

        | _ ->
            None

    let a_a2 =
        fun x ->
            match x with
            | ( recordYZ :: tail, None as nothing, ( Some [ "" ], 0.0 ) ) ->
                let y =
                    recordYZ.Y
                
                let z =
                    recordYZ.Z

                0.0

            | _ ->
                1.0
"""
            )
        ]


expectTranspiledToFsharpStringAs : String -> String -> Expect.Expectation
expectTranspiledToFsharpStringAs expected source =
    case
        [ source, elmCoreMaybeSourcePartial ]
            |> List.foldl
                (\moduleSource soFarOrError ->
                    case moduleSource |> Elm.Parser.parseToFile of
                        Err deadEnds ->
                            Err
                                (("failed to parse actual source: "
                                    ++ (deadEnds |> Debug.toString)
                                 )
                                    :: (case soFarOrError of
                                            Err errors ->
                                                errors

                                            Ok _ ->
                                                []
                                       )
                                )

                        Ok parsed ->
                            case soFarOrError of
                                Err error ->
                                    Err error

                                Ok soFar ->
                                    Ok (parsed :: soFar)
                )
                (Ok [])
    of
        Err deadEnds ->
            Expect.fail
                ("failed to parse actual source: "
                    ++ (deadEnds |> Debug.toString)
                )

        Ok parsedModules ->
            let
                transpiledResult :
                    { errors : List String
                    , declarations :
                        { valuesAndFunctions :
                            FastDict.Dict
                                String
                                { result : ElmSyntaxToFsharp.FsharpExpression
                                , type_ : ElmSyntaxToFsharp.FsharpType
                                }
                        , typeAliases :
                            FastDict.Dict
                                String
                                { parameters : List String
                                , type_ : ElmSyntaxToFsharp.FsharpType
                                }
                        , recordTypes : FastSet.Set (List String)
                        , choiceTypes :
                            FastDict.Dict
                                String
                                { parameters : List String
                                , variants : FastDict.Dict String (Maybe ElmSyntaxToFsharp.FsharpType)
                                }
                        }
                    }
                transpiledResult =
                    parsedModules |> ElmSyntaxToFsharp.modules
            in
            case transpiledResult.errors of
                transpilationError0 :: transpilationError1Up ->
                    Expect.fail
                        ("failed to transpile the parsed elm to fsharp: "
                            ++ ((transpilationError0 :: transpilationError1Up)
                                    |> String.join " and "
                               )
                        )

                [] ->
                    let
                        printed : String
                        printed =
                            transpiledResult.declarations
                                |> ElmSyntaxToFsharp.fsharpDeclarationsToModuleString
                    in
                    if printed == expected then
                        Expect.pass

                    else
                        Expect.fail
                            ("actual printed source is\n\n"
                                ++ printed
                                ++ "\n\nbut I expected\n\n"
                                ++ expected
                                ++ "\n\nThey differ in lines\n"
                                ++ (List.map2
                                        (\actualLine expectedLine -> { actual = actualLine, expected = expectedLine })
                                        (printed |> String.lines)
                                        (expected |> String.lines)
                                        |> List.indexedMap
                                            (\i lines ->
                                                if lines.actual == lines.expected then
                                                    Nothing

                                                else
                                                    Just
                                                        ((i |> String.fromInt)
                                                            ++ ": "
                                                            ++ (lines.actual
                                                                    |> String.replace " " "·"
                                                               )
                                                        )
                                            )
                                        |> List.filterMap identity
                                        |> List.take 10
                                        |> String.join "\n"
                                   )
                            )


elmCoreMaybeSourcePartial : String
elmCoreMaybeSourcePartial =
    """module Maybe exposing
  ( Maybe(..)
  , andThen
  , map, map2, map3, map4, map5
  , withDefault
  )


import Basics exposing (Bool(..))

type Maybe a
    = Just a
    | Nothing
"""
