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
                        , enumTypes :
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
