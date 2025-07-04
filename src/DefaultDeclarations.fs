module Elm

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
    let comparisonMagnitude: int = compare a b

    if comparisonMagnitude = 0 then Basics_Order.EQ
    else if comparisonMagnitude < 0 then Basics_Order.LT
    else Basics_Order.GT

let inline Basics_ceiling (n: float) : int64 = int64 (System.Double.Ceiling n)
let inline Basics_floor (n: float) : int64 = int64 (System.Double.Floor n)
let inline Basics_round (n: float) : int64 = int64 (System.Double.Round n)
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

    if (remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0) then
        remainder + toDivide

    else
        remainder

let inline Basics_fpow (a: float) (b: float) : float = a ** b

let inline Basics_ipow (a: int64) (b: int64) : int64 = int64 (float a ** float b)

let inline Basics_iclamp (minimum: int64) (maximum: int64) (n: int64) : int64 =
    System.Int64.Clamp(value = n, min = minimum, max = maximum)

let inline Basics_fclamp (minimum: float) (maximum: float) (n: float) : float =
    System.Double.Clamp(value = n, min = minimum, max = maximum)

let inline Basics_logBase (newBase: float) (n: float) : float =
    System.Double.Log(n, newBase = newBase)

let inline Basics_atan2 (y: float) (x: float) : float = System.Double.Atan2(y, x)

let inline Basics_radians (radians: float) : float = radians

let inline Basics_degrees (angleInDegrees: float) : float =
    (angleInDegrees * System.Double.Pi) / 180.0

let inline Basics_turns (angleInTurns: float) : float =
    (System.Double.Pi * 2.0) * angleInTurns

let Basics_fromPolar
    (struct (radius: float, theta: float))
    : struct (float * float) =
    struct (radius * System.Double.Cos theta, radius * System.Double.Sin theta)

let Basics_toPolar (struct (x: float, y: float)) : struct (float * float) =
    struct (System.Double.Sqrt((x * x) + (y * y)), System.Double.Atan2(y, x))

let inline Bitwise_shiftLeftBy (bitPositionsToShiftBy: int64) (n: int64) : int64 =
    int64 (int32 (n <<< int32 bitPositionsToShiftBy))

let inline Bitwise_shiftRightBy (bitPositionsToShiftBy: int64) (n: int64) : int64 =
    int64 (int32 (n >>> int32 bitPositionsToShiftBy))

let inline Bitwise_shiftRightZfBy
    (bitPositionsToShiftBy: int64)
    (n: int64)
    : int64 =
    int64 (uint32 n >>> int32 bitPositionsToShiftBy)

let inline Basics_and (a: bool) (b: bool) : bool = a && b
let inline Basics_or (a: bool) (b: bool) : bool = a || b

type Result_Result<'error, 'value> = Result<'value, 'error>

type Basics_Never = JustOneMore of Basics_Never
let rec Basics_never (JustOneMore ever: Basics_Never) = Basics_never ever

let inline Char_isOctDigit (rune: int) : bool = rune <= 0x37 && 0x30 <= rune

let inline Char_isHexDigit (rune: int) : bool =
    (0x30 <= rune && rune <= 0x39)
    || (0x41 <= rune && rune <= 0x46)
    || (0x61 <= rune && rune <= 0x66)

let inline Char_isDigit (rune: int) : bool = rune <= 0x39 && 0x30 <= rune
let inline Char_isUpper (rune: int) : bool = rune <= 0x5A && 0x41 <= rune
let inline Char_isLower (rune: int) : bool = 0x61 <= rune && rune <= 0x7A
let inline Char_isAlpha (rune: int) : bool = Char_isLower rune || Char_isUpper rune

let inline Char_isAlphaNum (rune: int) : bool =
    Char_isAlpha rune || Char_isDigit rune

let inline Char_toLocaleLower (rune: int) : int =
    (System.Text.Rune.ToLower(
        System.Text.Rune rune,
        System.Globalization.CultureInfo.CurrentCulture
    ))
        .Value

let inline Char_toLocaleUpper (rune: int) : int =
    (System.Text.Rune.ToUpper(
        System.Text.Rune rune,
        System.Globalization.CultureInfo.CurrentCulture
    ))
        .Value

let inline Char_toLower (rune: int) : int =
    (System.Text.Rune.ToLowerInvariant(System.Text.Rune rune)).Value

let inline Char_toUpper (rune: int) : int =
    (System.Text.Rune.ToUpperInvariant(System.Text.Rune rune)).Value

[<CustomEquality; CustomComparison>]
type StringRope =
    | StringRopeOne of string
    | StringRopeAppend of struct (StringRope * StringRope)

    static member inline fromString(string: string) : StringRope =
        StringRopeOne string

    static member toString(this: StringRope) : string =
        match this with
        | StringRopeOne content -> content
        | StringRopeAppend(fullLeftRope, fullRightRope) ->
            let mutableBuilder = System.Text.StringBuilder()
            let mutable stringRopeToMatchNext = fullLeftRope
            let mutable shouldKeepGoing = true

            let mutableRemainingRightStringRopes
                : System.Collections.Generic.Stack<StringRope> =
                System.Collections.Generic.Stack()

            mutableRemainingRightStringRopes.Push fullRightRope

            while shouldKeepGoing do
                match stringRopeToMatchNext with
                | StringRopeOne segment ->
                    let _ = mutableBuilder.Append segment

                    if mutableRemainingRightStringRopes.Count = 0 then
                        shouldKeepGoing <- false
                    else
                        stringRopeToMatchNext <-
                            mutableRemainingRightStringRopes.Pop()
                | StringRopeAppend(left, right) ->
                    stringRopeToMatchNext <- left
                    mutableRemainingRightStringRopes.Push right

            mutableBuilder.ToString()

    override x.GetHashCode() : int = hash (StringRope.toString x)

    override x.Equals(other: obj) : bool =
        match other with
        | :? StringRope as otherStringRope ->
            StringRope.toString x = StringRope.toString otherStringRope
        | _ -> false

    interface System.IComparable with
        member x.CompareTo other =
            match other with
            | :? StringRope as otherStringRope ->
                (StringRope.toString x)
                    .CompareTo(StringRope.toString otherStringRope)
            | _ -> -1

let stringRopeEmpty: StringRope = StringRopeOne ""

let rec String_isEmpty (stringToCheck: StringRope) : bool =
    match stringToCheck with
    | StringRopeOne string -> System.String.IsNullOrEmpty string
    | StringRopeAppend(left, right) -> String_isEmpty left && String_isEmpty right

let inline String_length (str: StringRope) : int64 =
    String.length (StringRope.toString str)

let inline String_repeat (repetitions: int64) (segment: StringRope) : StringRope =
    StringRope.fromString (
        String.replicate
            (System.Int32.Max(0, int repetitions))
            (StringRope.toString segment)
    )

let String_toList (stringRope: StringRope) : List<int> =
    Seq.toList (
        Seq.map
            (fun (rune: System.Text.Rune) -> rune.Value)
            ((StringRope.toString stringRope).EnumerateRunes())
    )

let runesToString (runes: seq<System.Text.Rune>) : string =
    let stringBuilder: System.Text.StringBuilder = System.Text.StringBuilder()

    for rune in runes do
        let _ =
            if rune.Utf16SequenceLength = 1 then
                stringBuilder.Append rune.Value
            else
                stringBuilder.Append(rune.ToString())

        ()

    stringBuilder.ToString()

let inline String_fromList (runes: List<int>) : StringRope =
    StringRope.fromString (
        runesToString (Seq.map (fun (code: int) -> System.Text.Rune code) runes)
    )

let inline String_contains (substringRope: StringRope) (string: StringRope) : bool =
    (StringRope.toString string).Contains(StringRope.toString substringRope)

let inline String_startsWith (start: StringRope) (string: StringRope) : bool =
    (StringRope.toString string).StartsWith(StringRope.toString start)

let inline String_endsWith (ending: StringRope) (string: StringRope) : bool =
    (StringRope.toString string).EndsWith(StringRope.toString ending)

let inline String_any
    ([<InlineIfLambda>] runeIsNeedle: int -> bool)
    (stringRope: StringRope)
    : bool =
    Seq.exists
        (fun (rune: System.Text.Rune) -> runeIsNeedle rune.Value)
        ((StringRope.toString stringRope).EnumerateRunes())

let inline String_all
    ([<InlineIfLambda>] runeIsExpected: int -> bool)
    (stringRope: StringRope)
    : bool =
    Seq.forall
        (fun (rune: System.Text.Rune) -> runeIsExpected rune.Value)
        ((StringRope.toString stringRope).EnumerateRunes())

let inline String_map
    ([<InlineIfLambda>] runeChange: int -> int)
    (string: StringRope)
    : StringRope =
    StringRope.fromString (
        runesToString (
            Seq.map
                (fun (rune: System.Text.Rune) ->
                    System.Text.Rune(runeChange rune.Value))
                ((StringRope.toString string).EnumerateRunes())
        )
    )

let inline String_filter
    ([<InlineIfLambda>] charShouldBeKept: int -> bool)
    (string: StringRope)
    : StringRope =
    StringRope.fromString (
        runesToString (
            Seq.filter
                (fun (rune: System.Text.Rune) -> charShouldBeKept rune.Value)
                ((StringRope.toString string).EnumerateRunes())
        )
    )

let inline String_foldl
    ([<InlineIfLambda>] reduce: int -> 'folded -> 'folded)
    (initialFolded: 'folded)
    (string: StringRope)
    : 'folded =
    Seq.fold
        (fun (soFar: 'folded) (rune: System.Text.Rune) -> reduce rune.Value soFar)
        initialFolded
        ((StringRope.toString string).EnumerateRunes())

let inline String_foldr
    ([<InlineIfLambda>] reduce: int -> 'folded -> 'folded)
    (initialFolded: 'folded)
    (string: StringRope)
    : 'folded =
    Seq.foldBack
        (fun (rune: System.Text.Rune) (soFar: 'folded) -> reduce rune.Value soFar)
        ((StringRope.toString string).EnumerateRunes())
        initialFolded

let inline String_trim (string: StringRope) : StringRope =
    StringRope.fromString ((StringRope.toString string).Trim())

let inline String_trimLeft (string: StringRope) : StringRope =
    StringRope.fromString ((StringRope.toString string).TrimStart())

let inline String_trimRight (string: StringRope) : StringRope =
    StringRope.fromString ((StringRope.toString string).TrimEnd())

let String_right
    (takenElementCountInt64: int64)
    (stringRope: StringRope)
    : StringRope =
    let string: string = StringRope.toString stringRope

    let takenElementCount: int =
        System.Int32.Clamp(
            value = int takenElementCountInt64,
            min = 0,
            max = String.length string
        )


    StringRope.fromString (
        string.Substring(
            String.length string - takenElementCount,
            takenElementCount
        )
    )

let String_left (skippedElementCount: int64) (stringRope: StringRope) : StringRope =
    let string = StringRope.toString stringRope

    StringRope.fromString (
        string.Substring(
            0,
            System.Int32.Clamp(
                value = int skippedElementCount,
                min = 0,
                max = String.length string
            )
        )
    )

let String_dropRight
    (skippedElementCount: int64)
    (stringRope: StringRope)
    : StringRope =
    let string: string = StringRope.toString stringRope

    StringRope.fromString (
        string.Substring(
            0,
            System.Int32.Clamp(
                value = String.length string - int skippedElementCount,
                min = 0,
                max = String.length string
            )
        )
    )

let String_dropLeft
    (skippedElementCountInt64: int64)
    (stringRope: StringRope)
    : StringRope =
    let string: string = StringRope.toString stringRope

    let skippedElementCount: int =
        System.Int32.Clamp(
            value = int skippedElementCountInt64,
            min = 0,
            max = String.length string
        )

    StringRope.fromString (
        string.Substring(
            skippedElementCount,
            String.length string - skippedElementCount
        )
    )

let inline String_append (early: StringRope) (late: StringRope) : StringRope =
    StringRopeAppend(early, late)

let inline String_fromChar (rune: int) : StringRope =
    StringRope.fromString ((System.Text.Rune rune).ToString())

let inline String_cons (newHeadChar: int) (late: StringRope) : StringRope =
    StringRopeAppend(String_fromChar newHeadChar, late)

let String_uncons
    (stringRope: StringRope)
    : ValueOption<struct (int * StringRope)> =
    let string: string = StringRope.toString stringRope

    match String.length string with
    | 0 -> ValueNone
    | 1 -> ValueSome(struct (int (string[0]), stringRopeEmpty))
    | _ ->
        if System.Char.IsSurrogate(string[0]) then
            ValueSome(
                struct (System.Char.ConvertToUtf32(string[0], string[1]),
                        StringRope.fromString (string[2..]))
            )
        else
            ValueSome(struct (int (string[0]), StringRope.fromString (string[1..])))

let String_split (separator: StringRope) (string: StringRope) : List<StringRope> =
    // can be optimized
    Seq.toList (
        Seq.map
            StringRope.fromString
            ((StringRope.toString string).Split(StringRope.toString separator))
    )

let newLineOptions: array<string> = [| "\r\n"; "\n" |]

let String_lines (string: StringRope) : List<StringRope> =
    // can be optimized
    Seq.toList (
        Seq.map
            StringRope.fromString
            ((StringRope.toString string)
                .Split(newLineOptions, System.StringSplitOptions.None))
    )

let whitespaceCharacters: array<char> =
    // \s in https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions/Cheatsheet
    [| '\n'
       '\r'
       '\f'
       '\t'
       '\v'
       ' '
       '\u00a0'
       '\u1680'
       '\u2000'
       '\u2001'
       '\u2002'
       '\u2003'
       '\u2004'
       '\u2005'
       '\u2006'
       '\u2007'
       '\u2008'
       '\u2009'
       '\u200a'
       '\u2028'
       '\u2029'
       '\u202f'
       '\u205f'
       '\u3000'
       '\ufeff' |]

let String_words (string: StringRope) : List<StringRope> =
    // can be optimized
    Seq.toList (
        Seq.map
            StringRope.fromString
            ((StringRope.toString string)
                .Split(
                    whitespaceCharacters,
                    System.StringSplitOptions.RemoveEmptyEntries
                ))
    )

let String_reverse (string: StringRope) : StringRope =
    StringRope.fromString (
        new string (Array.rev ((StringRope.toString string).ToCharArray()))
    )

let inline String_replace
    (toReplace: StringRope)
    (replacement: StringRope)
    (string: StringRope)
    : StringRope =
    StringRope.fromString (
        (StringRope.toString string)
            .Replace(StringRope.toString toReplace, StringRope.toString replacement)
    )

let inline String_toUpper (string: StringRope) : StringRope =
    StringRope.fromString ((StringRope.toString string).ToUpperInvariant())

let inline String_toLower (string: StringRope) : StringRope =
    StringRope.fromString ((StringRope.toString string).ToLowerInvariant())

let inline String_join
    (separator: StringRope)
    (strings: List<StringRope>)
    : StringRope =
    StringRope.fromString (
        String.concat
            (StringRope.toString separator)
            (Seq.map StringRope.toString strings)
    )

let inline String_concat (strings: List<StringRope>) : StringRope =
    StringRope.fromString (
        System.String.Concat(Seq.map StringRope.toString strings)
    )

let inline String_padLeft
    (newMinimumLength: int64)
    (padding: int)
    (string: StringRope)
    : StringRope =
    StringRope.fromString (
        (StringRope.toString string)
            .PadLeft(System.Int32.Max(0, int newMinimumLength), char padding)
    )

let inline String_padRight
    (newMinimumLength: int64)
    (padding: int)
    (string: StringRope)
    : StringRope =
    StringRope.fromString (
        (StringRope.toString string)
            .PadRight(System.Int32.Max(0, int newMinimumLength), char padding)
    )

let inline String_fromFloat (n: float) : StringRope =
    StringRope.fromString (string n)

let inline String_fromInt (n: int64) : StringRope = StringRope.fromString (string n)

let String_toInt (string: StringRope) : ValueOption<int64> =
    let (success, num) = System.Int64.TryParse(StringRope.toString string)

    if success then ValueSome num else ValueNone

let String_toFloat (string: StringRope) : ValueOption<float> =
    let (success, num) = System.Double.TryParse(StringRope.toString string)

    if success then ValueSome num else ValueNone

let String_slice
    (startInclusivePossiblyNegative: int64)
    (endExclusivePossiblyNegative: int64)
    (stringRope: StringRope)
    : StringRope =
    let string = StringRope.toString stringRope

    let realStartIndex: int =
        if startInclusivePossiblyNegative < 0L then
            System.Int32.Max(
                0,
                int startInclusivePossiblyNegative + String.length string
            )
        else
            int startInclusivePossiblyNegative

    let realEndIndexExclusive: int =
        if endExclusivePossiblyNegative < 0L then
            System.Int32.Max(
                0,
                int endExclusivePossiblyNegative + String.length string
            )
        else
            System.Int32.Min(int endExclusivePossiblyNegative, String.length string)

    if realStartIndex >= realEndIndexExclusive then
        stringRopeEmpty
    else
        StringRope.fromString (
            string.Substring(realStartIndex, realEndIndexExclusive - realStartIndex)
        )

let inline List_length (list: List<'a>) : int64 = List.length list

let inline List_head (list: List<'a>) : ValueOption<'a> =
    match list with
    | [] -> ValueNone
    | head :: _ -> ValueSome head

let inline List_tail (list: List<'a>) : ValueOption<List<'a>> =
    match list with
    | [] -> ValueNone
    | _head :: tail -> ValueSome tail

let inline List_filterMap
    ([<InlineIfLambda>] elementToMaybe: 'a -> ValueOption<'b>)
    (list: List<'a>)
    : List<'b> =
    List.choose (fun element -> ValueOption.toOption (elementToMaybe element)) list

let inline List_member (needle: 'a) (list: List<'a>) : bool =
    List.contains needle list

let inline List_minimum (list: List<'a>) : ValueOption<'a> =
    match list with
    | [] -> ValueNone
    | _ :: _ -> ValueSome(List.min list)

let inline List_maximum (list: List<'a>) : ValueOption<'a> =
    match list with
    | [] -> ValueNone
    | _ :: _ -> ValueSome(List.max list)

let inline List_fproduct (list: List<float>) : float = List.fold (*) 1.0 list
let inline List_iproduct (list: List<int64>) : int64 = List.fold (*) 1L list

let inline List_cons (newHead: 'a) (tail: List<'a>) : List<'a> = newHead :: tail

let inline List_repeat (count: int64) (element: 'a) : List<'a> =
    List.replicate (System.Int32.Max(0, int count)) element

let inline List_take (elementCountFromStart: int64) (list: List<'a>) : List<'a> =
    List.truncate (int elementCountFromStart) list

let inline List_drop
    (skippedElementCountFromStart: int64)
    (list: List<'a>)
    : List<'a> =
    try
        List.skip (int skippedElementCountFromStart) list
    with _ ->
        []

let inline List_sortWith
    ([<InlineIfLambda>] elementCompare: 'a -> 'a -> Basics_Order)
    (list: List<'a>)
    : List<'a> =
    List.sortWith (fun a b -> int (elementCompare a b)) list

let List_intersperse (sep: 'a) (list: List<'a>) : List<'a> =
    match list with
    | [] -> []
    | listHead :: listTail ->
        List.foldBack (fun x soFar -> x :: sep :: soFar) listTail [ listHead ]

let inline List_foldl
    ([<InlineIfLambda>] reduce: 'a -> 'state -> 'state)
    (initialState: 'state)
    (list: List<'a>)
    : 'state =
    List.fold (fun soFar element -> reduce element soFar) initialState list

let inline List_foldr
    ([<InlineIfLambda>] reduce: 'a -> 'state -> 'state)
    (initialState: 'state)
    (list: List<'a>)
    : 'state =
    List.foldBack reduce list initialState

let inline List_range (startFloat: int64) (endFloat: int64) : List<int64> =
    [ startFloat..endFloat ]

let inline List_indexedMap
    ([<InlineIfLambda>] indexAndElementToNewElement: int64 -> 'a -> 'b)
    (list: List<'a>)
    : List<'b> =
    List.mapi
        (fun index element -> indexAndElementToNewElement (int64 index) element)
        list

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
                        (combine aHead bHead cHead dHead :: combinedSoFarReverse)
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
                             :: combinedSoFarReverse)
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


let inline Maybe_map4
    ([<InlineIfLambda>] valuesCombine: 'a -> 'b -> 'c -> 'd -> 'combined)
    (aOption: ValueOption<'a>)
    (bOption: ValueOption<'b>)
    (cOption: ValueOption<'c>)
    (dOption: ValueOption<'d>)
    : ValueOption<'combined> =
    match aOption, bOption, cOption, dOption with
    | ValueSome a, ValueSome b, ValueSome c, ValueSome d ->
        ValueSome(valuesCombine a b c d)
    | _ -> ValueNone

let inline Maybe_map5
    ([<InlineIfLambda>] valuesCombine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined)
    (aOption: ValueOption<'a>)
    (bOption: ValueOption<'b>)
    (cOption: ValueOption<'c>)
    (dOption: ValueOption<'d>)
    (eOption: ValueOption<'e>)
    : ValueOption<'combined> =
    match aOption, bOption, cOption, dOption, eOption with
    | ValueSome a, ValueSome b, ValueSome c, ValueSome d, ValueSome e ->
        ValueSome(valuesCombine a b c d e)
    | _ -> ValueNone


let inline Result_map2
    ([<InlineIfLambda>] valuesCombine: 'a -> 'b -> 'combined)
    (aResult: Result<'a, 'error>)
    (bResult: Result<'b, 'error>)
    : Result<'combined, 'error> =
    match aResult with
    | Error error -> Error error
    | Ok a ->
        match bResult with
        | Error error -> Error error
        | Ok b -> Ok(valuesCombine a b)

let inline Result_map3
    ([<InlineIfLambda>] valuesCombine: 'a -> 'b -> 'c -> 'combined)
    (aResult: Result<'a, 'error>)
    (bResult: Result<'b, 'error>)
    (cResult: Result<'c, 'error>)
    : Result<'combined, 'error> =
    match aResult with
    | Error error -> Error error
    | Ok a ->
        match bResult with
        | Error error -> Error error
        | Ok b ->
            match cResult with
            | Error error -> Error error
            | Ok c -> Ok(valuesCombine a b c)

let inline Result_map4
    ([<InlineIfLambda>] valuesCombine: 'a -> 'b -> 'c -> 'd -> 'combined)
    (aResult: Result<'a, 'error>)
    (bResult: Result<'b, 'error>)
    (cResult: Result<'c, 'error>)
    (dResult: Result<'d, 'error>)
    : Result<'combined, 'error> =
    match aResult with
    | Error error -> Error error
    | Ok a ->
        match bResult with
        | Error error -> Error error
        | Ok b ->
            match cResult with
            | Error error -> Error error
            | Ok c ->
                match dResult with
                | Error error -> Error error
                | Ok d -> Ok(valuesCombine a b c d)

let inline Result_map5
    ([<InlineIfLambda>] valuesCombine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined)
    (aResult: Result<'a, 'error>)
    (bResult: Result<'b, 'error>)
    (cResult: Result<'c, 'error>)
    (dResult: Result<'d, 'error>)
    (eResult: Result<'e, 'error>)
    : Result<'combined, 'error> =
    match aResult with
    | Error error -> Error error
    | Ok a ->
        match bResult with
        | Error error -> Error error
        | Ok b ->
            match cResult with
            | Error error -> Error error
            | Ok c ->
                match dResult with
                | Error error -> Error error
                | Ok d ->
                    match eResult with
                    | Error error -> Error error
                    | Ok e -> Ok(valuesCombine a b c d e)

let inline Result_fromMaybe
    (errorOnNothing: 'error)
    (maybe: ValueOption<'value>)
    : Result<'value, 'error> =
    match maybe with
    | ValueNone -> Error errorOnNothing
    | ValueSome value -> Ok value


let inline Dict_size (dict: Map<'key, 'value>) : int64 = Map.count dict

let inline Dict_singleton (key: 'key) (value: 'value) : Map<'key, 'value> =
    Map [ (key, value) ]

let inline Dict_toList (dict: Map<'key, 'value>) : List<struct ('key * 'value)> =
    Map.foldBack (fun key value soFar -> (struct (key, value)) :: soFar) dict []

let inline Dict_fromList
    (keyValuePairs: List<struct ('key * 'value)>)
    : Map<'key, 'value> =
    List.fold
        (fun soFar (struct (key, value)) -> Map.add key value soFar)
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

let inline Dict_get (key: 'key) (dict: Map<'key, 'value>) : ValueOption<'value> =
    Option.toValueOption (Map.tryFind key dict)

let inline Dict_update
    (key: 'key)
    ([<InlineIfLambda>] slotChange: ValueOption<'value> -> ValueOption<'value>)
    (dict: Map<'key, 'value>)
    : Map<'key, 'value> =
    Map.change
        key
        (fun slot -> ValueOption.toOption (slotChange (Option.toValueOption slot)))
        dict

let inline Dict_keys (dict: Map<'key, 'value>) : List<'key> =
    Seq.toList (Map.keys dict)

let inline Dict_values (dict: Map<'key, 'value>) : List<'value> =
    Seq.toList (Map.values dict)

let Dict_diff
    (baseDict: Map<'key, 'a>)
    (dictWithKeysToRemove: Map<'key, 'b>)
    : Map<'key, 'a> =
    Map.fold (fun soFar k _ -> Map.remove k soFar) baseDict dictWithKeysToRemove

let Dict_union (aDict: Map<'key, 'a>) (bDict: Map<'key, 'a>) : Map<'key, 'a> =
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
    let rec stepState
        (list: List<('comparable * 'aValue)>, result: 'result)
        (rKey: 'comparable)
        (rValue: 'bValue)
        : (List<('comparable * 'aValue)> * 'result) =
        match list with
        | [] -> (list, rightStep rKey rValue result)

        | (lKey, lValue) :: rest ->
            if lKey < rKey then
                stepState (rest, leftStep lKey lValue result) rKey rValue

            else if lKey > rKey then
                (list, rightStep rKey rValue result)

            else
                (rest, bothStep lKey lValue rValue result)

    let (leftovers: List<('comparable * 'aValue)>, intermediateResult: 'result) =
        Map.fold stepState (Map.toList leftDict, initialResult) rightDict

    List.fold
        (fun result (k, v) -> leftStep k v result)
        intermediateResult
        leftovers

let inline Set_size (set: Set<'element>) : int64 = Set.count set

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

let inline Array_length (array: array<'a>) : int64 = Array.length array

let Array_get (index: int64) (array: array<'element>) : ValueOption<'element> =
    Option.toValueOption (Array.tryItem (int index) array)

let inline Array_initialize
    (count: int64)
    ([<InlineIfLambda>] indexToElement: int64 -> 'element)
    : array<'element> =
    Array.init (System.Int32.Max(0, int count)) (fun index -> indexToElement index)

let inline Array_repeat (count: int64) (element: 'element) : array<'element> =
    Array.replicate (System.Int32.Max(0, int count)) element

let Array_set
    (index: int64)
    (replacementElement: 'element)
    (array: array<'element>)
    : array<'element> =
    if (index < 0) || (index >= Array.length array) then
        array
    else
        Array.updateAt (int index) replacementElement array

let Array_push
    (newLastElement: 'element)
    (array: array<'element>)
    : array<'element> =
    Array.insertAt (Array.length array) newLastElement array

let inline Array_indexedMap
    ([<InlineIfLambda>] elementChange: int64 -> 'a -> 'b)
    (array: array<'a>)
    : array<'b> =
    Array.mapi (fun index element -> elementChange index element) array

let Array_toIndexedList (array: array<'a>) : List<struct (int64 * 'a)> =
    Seq.toList (
        Seq.mapi (fun (index: int) (element: 'a) -> struct (index, element)) array
    )

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
            System.Int32.Max(
                0,
                int startInclusivePossiblyNegative + Array.length array
            )
        else
            int startInclusivePossiblyNegative

    let realEndIndexExclusive: int =
        if (endExclusivePossiblyNegative < 0L) then
            System.Int32.Max(
                0,
                int endExclusivePossiblyNegative + Array.length array
            )
        else
            System.Int32.Min(int endExclusivePossiblyNegative, Array.length array)

    if realStartIndex >= realEndIndexExclusive then
        Array.empty
    else
        Array.sub array realStartIndex (realEndIndexExclusive - realStartIndex)


let JsonEncode_null: System.Text.Json.Nodes.JsonNode =
    System.Text.Json.Nodes.JsonValue.Create null

let inline JsonEncode_bool (bool: bool) : System.Text.Json.Nodes.JsonNode =
    System.Text.Json.Nodes.JsonValue.Create bool

let inline JsonEncode_stringRaw (string: string) : System.Text.Json.Nodes.JsonNode =
    System.Text.Json.Nodes.JsonValue.Create string

let inline JsonEncode_string
    (string: StringRope)
    : System.Text.Json.Nodes.JsonNode =
    JsonEncode_stringRaw(StringRope.toString string)

let inline JsonEncode_int (int: int64) : System.Text.Json.Nodes.JsonNode =
    System.Text.Json.Nodes.JsonValue.Create int

let inline JsonEncode_float (float: float) : System.Text.Json.Nodes.JsonNode =
    System.Text.Json.Nodes.JsonValue.Create float

let inline JsonEncode_list
    ([<InlineIfLambda>] elementToValue: 'element -> System.Text.Json.Nodes.JsonNode)
    (elements: List<'element>)
    : System.Text.Json.Nodes.JsonNode =
    // can be optimized
    System.Text.Json.Nodes.JsonArray(
        Array.ofList (List.map elementToValue elements)
    )

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
    System.Text.Json.Nodes.JsonArray(
        Array.map elementToValue (Set.toArray elements)
    )

let inline JsonEncode_object
    (fields: List<struct (StringRope * System.Text.Json.Nodes.JsonNode)>)
    : System.Text.Json.Nodes.JsonNode =
    System.Text.Json.Nodes.JsonObject(
        List.fold
            (fun soFar (struct (fieldName, fieldValue)) ->
                Map.add (StringRope.toString fieldName) fieldValue soFar)
            Map.empty
            fields
    )

let inline JsonEncode_objectRaw
    (fields: List<struct (string * System.Text.Json.Nodes.JsonNode)>)
    : System.Text.Json.Nodes.JsonNode =
    System.Text.Json.Nodes.JsonObject(
        List.fold
            (fun soFar (struct (fieldName, fieldValue)) ->
                Map.add fieldName fieldValue soFar)
            Map.empty
            fields
    )

let inline JsonEncode_dict
    ([<InlineIfLambda>] keyToString: 'key -> StringRope)
    ([<InlineIfLambda>] valueToJson: 'value -> System.Text.Json.Nodes.JsonNode)
    (dict: Map<'key, 'value>)
    : System.Text.Json.Nodes.JsonNode =
    System.Text.Json.Nodes.JsonObject(
        Map.fold
            (fun soFar key value ->
                Map.add
                    (StringRope.toString (keyToString key))
                    (valueToJson value)
                    soFar)
            Map.empty
            dict
    )

let lineSetIndentSizeFrom2To (newIndentSize: int) (line: string) : string =
    let lineWithoutIndentation: string = line.TrimStart ' '

    let lineIndentation: int =
        (String.length line - String.length lineWithoutIndentation) / 2

    String.replicate (int newIndentSize * lineIndentation) " "
    + lineWithoutIndentation

let setIndentSizeFrom2To (newIndentSize: int) (printed: string) : string =
    if newIndentSize = 2 then
        printed
    else
        String.concat
            "\n"
            (Array.map
                (fun (line: string) -> lineSetIndentSizeFrom2To newIndentSize line)
                (printed.Split '\n'))

let JsonEncode_encode
    (indentDepth: int64)
    (json: System.Text.Json.Nodes.JsonNode)
    : StringRope =
    let printOptions = System.Text.Json.JsonSerializerOptions()

    if (indentDepth = 0) then
        StringRope.fromString (json.ToJsonString printOptions)
    else
        printOptions.WriteIndented <- true
        // JsonSerializerOptions.IndentSize is only available since .net9.0
        StringRope.fromString (
            setIndentSizeFrom2To (int indentDepth) (json.ToJsonString(printOptions))
        )

type JsonDecode_Error =
    | JsonDecode_Field of (struct (StringRope * JsonDecode_Error))
    | JsonDecode_Index of (struct (int64 * JsonDecode_Error))
    | JsonDecode_OneOf of List<JsonDecode_Error>
    | JsonDecode_Failure of (struct (StringRope * System.Text.Json.Nodes.JsonNode))

type JsonDecode_Decoder<'value> =
    System.Text.Json.Nodes.JsonNode -> Result<'value, JsonDecode_Error>

let inline JsonDecode_decodeValue
    (decoder: JsonDecode_Decoder<'value>)
    (value: System.Text.Json.Nodes.JsonNode)
    : Result<'value, JsonDecode_Error> =
    decoder value

let inline JsonDecode_decodeString
    (decoder: JsonDecode_Decoder<'value>)
    (string: StringRope)
    : Result<'value, JsonDecode_Error> =
    try
        decoder (System.Text.Json.Nodes.JsonNode.Parse(StringRope.toString string))
    with :? System.Text.Json.JsonException ->
        Error(
            JsonDecode_Failure(
                StringRope.fromString "This is not valid JSON!",
                System.Text.Json.Nodes.JsonValue.Create(StringRope.toString string)
            )
        )

let inline JsonDecode_succeed (value: 'value) : JsonDecode_Decoder<'value> =
    fun _ -> Ok value

let inline JsonDecode_fail (errorMessage: StringRope) : JsonDecode_Decoder<'value> =
    fun jsonDomNode -> Error(JsonDecode_Failure(errorMessage, jsonDomNode))

let inline JsonDecode_map
    ([<InlineIfLambda>] valueChange: 'a -> 'b)
    (decoder: JsonDecode_Decoder<'a>)
    : JsonDecode_Decoder<'b> =
    fun jsonDomNode ->
        match decoder jsonDomNode with
        | Error error -> Error error
        | Ok value -> Ok(valueChange value)

let JsonDecode_lazy
    (lazilyConstructDecoder: unit -> JsonDecode_Decoder<'value>)
    : JsonDecode_Decoder<'value> =
    fun json -> lazilyConstructDecoder () json

let inline JsonDecode_andThen
    ([<InlineIfLambda>] decoderBasedOnValue: 'a -> JsonDecode_Decoder<'b>)
    (decoder: JsonDecode_Decoder<'a>)
    : JsonDecode_Decoder<'b> =
    fun json ->
        match decoder json with
        | Error error -> Error error
        | Ok value -> decoderBasedOnValue value json

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
            | Ok b -> Ok(combine a b)

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
                | Ok c -> Ok(combine a b c)

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
                    | Ok d -> Ok(combine a b c d)

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
                        | Ok e -> Ok(combine a b c d e)

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
                            | Ok f -> Ok(combine a b c d e f)

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
                                | Ok g -> Ok(combine a b c d e f g)

let inline JsonDecode_map8
    ([<InlineIfLambda>] combine:
        'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'combined)
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
                                    | Ok h -> Ok(combine a b c d e f g h)

let JsonDecode_maybe
    (valueDecoder: JsonDecode_Decoder<'value>)
    : JsonDecode_Decoder<ValueOption<'value>> =
    fun json ->
        Ok(
            match valueDecoder json with
            | Ok valueDecodeResult -> ValueSome valueDecodeResult
            | Error _ -> ValueNone
        )

let rec JsonDecode_oneOfWithErrorsReverse
    (errorsReverse: List<JsonDecode_Error>)
    (options: List<JsonDecode_Decoder<'value>>)
    : JsonDecode_Decoder<'value> =
    fun json ->
        match options with
        | [] -> Error(JsonDecode_OneOf(List.rev errorsReverse))
        | nextOptionToTry :: remainingOptions ->
            match nextOptionToTry json with
            | Ok value -> Ok value
            | Error error ->
                JsonDecode_oneOfWithErrorsReverse
                    (error :: errorsReverse)
                    remainingOptions
                    json

let JsonDecode_oneOf
    (options: List<JsonDecode_Decoder<'value>>)
    : JsonDecode_Decoder<'value> =
    JsonDecode_oneOfWithErrorsReverse [] options


let JsonDecode_value: JsonDecode_Decoder<System.Text.Json.Nodes.JsonNode> =
    fun json -> Ok json

let JsonDecode_stringRaw: JsonDecode_Decoder<string> =
    fun json ->
        try
            Ok(json.AsValue().GetValue<string>())
        with _ ->
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString "Expecting a STRING", json)
                )
            )

let JsonDecode_string: JsonDecode_Decoder<StringRope> =
    fun json ->
        try
            Ok(StringRope.fromString (json.AsValue().GetValue<string>()))
        with _ ->
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString "Expecting a STRING", json)
                )
            )

let JsonDecode_int: JsonDecode_Decoder<int64> =
    fun json ->
        try
            Ok(json.AsValue().GetValue<int64>())
        with _ ->
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString "Expecting an INT", json)
                )
            )

let JsonDecode_float: JsonDecode_Decoder<float> =
    fun json ->
        try
            Ok(json.AsValue().GetValue<float>())
        with _ ->
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString "Expecting a FLOAT", json)
                )
            )

let JsonDecode_bool: JsonDecode_Decoder<bool> =
    fun json ->
        try
            Ok(json.AsValue().GetValue<bool>())
        with _ ->
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString "Expecting a BOOL", json)
                )
            )

let inline JsonDecode_null (value: 'value) : JsonDecode_Decoder<'value> =
    fun json ->
        if json = JsonEncode_null then
            Ok value
        else
            match json.GetValueKind() with
            | System.Text.Json.JsonValueKind.Null -> Ok value
            | _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRope.fromString "Expecting NULL", json)
                    )
                )

let JsonDecode_index
    (index: int64)
    (elementDecoder: JsonDecode_Decoder<'element>)
    : JsonDecode_Decoder<'element> =
    fun json ->
        if index < 0 then
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString (
                                "Expecting an element at array index "
                                + string index
                                + " (likely a logic error in decoder code)"
                            ),
                            json)
                )
            )
        else
            try
                let jsonArray: System.Text.Json.Nodes.JsonArray = json.AsArray()

                if index >= jsonArray.Count then
                    Error(
                        JsonDecode_Failure(
                            struct (StringRope.fromString (
                                        "Expecting a LONGER array. Need index "
                                        + string index
                                        + " but only see "
                                        + string jsonArray.Count
                                        + " elements"
                                    ),
                                    json)
                        )
                    )
                else
                    match elementDecoder (jsonArray[int index]) with
                    | Ok elementDecoded -> Ok elementDecoded
                    | Error error -> Error(JsonDecode_Index(struct (index, error)))
            with _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRope.fromString "Expecting an ARRAY", json)
                    )
                )

let JsonDecode_list
    (elementDecoder: JsonDecode_Decoder<'element>)
    : JsonDecode_Decoder<List<'element>> =
    fun json ->
        try
            let jsonArray: System.Text.Json.Nodes.JsonArray = json.AsArray()

            let folded =
                Seq.foldBack
                    (fun
                        element
                        (soFar:
                            {| Index: int64
                               Result: Result<List<'element>, JsonDecode_Error> |}) ->
                        match soFar.Result with
                        | Error _ -> soFar
                        | Ok tail ->
                            {| Index = soFar.Index - 1L
                               Result =
                                match elementDecoder element with
                                | Error error ->
                                    Error(
                                        JsonDecode_Index(
                                            struct (soFar.Index, error)
                                        )
                                    )
                                | Ok head -> Ok(head :: tail) |})
                    jsonArray
                    {| Index = int64 jsonArray.Count
                       Result = Ok [] |}

            folded.Result
        with _ ->
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString "Expecting a LIST", json)
                )
            )

let JsonDecode_array
    (elementDecoder: JsonDecode_Decoder<'element>)
    : JsonDecode_Decoder<array<'element>> =
    // can be optimized
    JsonDecode_map Array.ofList (JsonDecode_list elementDecoder)

let JsonDecode_fieldRaw
    (fieldNameString: string)
    (valueDecoder: JsonDecode_Decoder<'value>)
    : JsonDecode_Decoder<'value> =
    fun json ->
        try
            let jsonObject: System.Text.Json.Nodes.JsonObject = json.AsObject()

            match jsonObject[fieldNameString] with
            | null ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRope.fromString (
                                    "Expecting an OBJECT with a field named '"
                                    + fieldNameString
                                    + "'"
                                ),
                                json)
                    )
                )
            | fieldValueJson ->
                match valueDecoder fieldValueJson with
                | Ok fieldValue -> Ok fieldValue
                | Error error ->
                    Error(
                        JsonDecode_Field(
                            struct (StringRope.fromString fieldNameString, error)
                        )
                    )
        with _ ->
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString (
                                "Expecting an OBJECT with a field named '"
                                + fieldNameString
                                + "'"
                            ),
                            json)
                )
            )

let inline JsonDecode_field
    (fieldNameStringRope: StringRope)
    (valueDecoder: JsonDecode_Decoder<'value>)
    : JsonDecode_Decoder<'value> =
    JsonDecode_fieldRaw (StringRope.toString fieldNameStringRope) valueDecoder

let JsonDecode_at
    (fieldNames: List<StringRope>)
    (valueDecoder: JsonDecode_Decoder<'value>)
    : JsonDecode_Decoder<'value> =
    List.foldBack
        (fun (fieldName: StringRope) (decoderSoFar: JsonDecode_Decoder<'value>) ->
            JsonDecode_field fieldName decoderSoFar)
        fieldNames
        valueDecoder

let JsonDecode_dict
    (valueDecoder: JsonDecode_Decoder<'value>)
    : JsonDecode_Decoder<Map<StringRope, 'value>> =
    fun json ->
        try
            let jsonObject: System.Text.Json.Nodes.JsonObject = json.AsObject()

            Seq.foldBack
                (fun
                    (field:
                        System.Collections.Generic.KeyValuePair<
                            string,
                            System.Text.Json.Nodes.JsonNode
                         >)
                    (soFarOrError: Result<Map<StringRope, 'value>, JsonDecode_Error>) ->
                    match soFarOrError with
                    | Error _ -> soFarOrError
                    | Ok soFar ->
                        match valueDecoder field.Value with
                        | Error error ->
                            Error(
                                JsonDecode_Field(
                                    struct (StringRope.fromString field.Key, error)
                                )
                            )
                        | Ok fieldValue ->
                            Ok(
                                Map.add
                                    (StringRope.fromString field.Key)
                                    fieldValue
                                    soFar
                            ))
                jsonObject
                (Ok Map.empty)
        with _ ->
            Error(
                JsonDecode_Failure(
                    struct (StringRope.fromString "Expecting an OBJECT", json)
                )
            )

let JsonDecode_keyValuePairs
    (valueDecoder: JsonDecode_Decoder<'value>)
    : JsonDecode_Decoder<List<struct (StringRope * 'value)>> =
    // can be optimized
    JsonDecode_map Dict_toList (JsonDecode_dict valueDecoder)

let JsonDecode_nullable
    (valueDecoder: JsonDecode_Decoder<'value>)
    : JsonDecode_Decoder<ValueOption<'value>> =
    fun json ->
        match JsonDecode_null ValueNone json with
        | Ok nullDecodeResult -> Ok nullDecodeResult
        | Error nullError ->
            match valueDecoder json with
            | Ok valueDecodeResult -> Ok(ValueSome valueDecodeResult)
            | Error valueError -> Error(JsonDecode_OneOf [ nullError; valueError ])

let inline JsonDecode_oneOrMore
    ([<InlineIfLambda>] combineHeadTail: 'element -> List<'element> -> 'combined)
    (elementDecoder: JsonDecode_Decoder<'element>)
    : JsonDecode_Decoder<'combined> =
    JsonDecode_map2 combineHeadTail elementDecoder (JsonDecode_list elementDecoder)

let inline indent (str: string) : string =
    String.concat "\n    " (Array.toList (str.Split "\n"))

let rec JsonDecode_errorToStringHelp
    (error: JsonDecode_Error)
    (context: List<string>)
    : string =
    match error with
    | JsonDecode_Field(f, err) ->
        let isSimple =
            match String_uncons f with
            | ValueNone -> false
            | ValueSome(head, rest) ->
                Char_isAlpha head && String_all Char_isAlphaNum rest

        let fieldName =
            if isSimple then
                "." + StringRope.toString f
            else
                "['" + StringRope.toString f + "']"

        JsonDecode_errorToStringHelp err (fieldName :: context)

    | JsonDecode_Index(i, err) ->
        let indexName = "[" + string i + "]"

        JsonDecode_errorToStringHelp err (indexName :: context)

    | JsonDecode_OneOf(errors) ->
        match errors with
        | [] ->
            "Ran into a Json.Decode.oneOf with no possibilities"
            + (match context with
               | [] -> "!"
               | _ :: _ -> " at json" + String.concat "" (List.rev context))

        | [ err ] -> JsonDecode_errorToStringHelp err context

        | _ :: _ :: _ ->
            let starter =
                match context with
                | [] -> "Json.Decode.oneOf"
                | _ :: _ ->
                    "The Json.Decode.oneOf at json"
                    + String.concat "" (List.rev context)

            let introduction =
                starter
                + " failed in the following "
                + string (List.length errors)
                + " ways:"

            String.concat
                "\n\n"
                (introduction
                 :: List.mapi
                     (fun (i: int32) (error: JsonDecode_Error) ->
                         "\n\n("
                         + string (i + 1)
                         + ") "
                         + indent (JsonDecode_errorToStringHelp error []))
                     errors)

    | JsonDecode_Failure(msg, json) ->
        let introduction =
            match context with
            | [] -> "Problem with the given value:\n\n"
            | _ :: _ ->
                "Problem with the value at json"
                + String.concat "" (List.rev context)
                + ":\n\n    "

        introduction
        + indent (StringRope.toString (JsonEncode_encode 4 json))
        + "\n\n"
        + StringRope.toString msg

let JsonDecode_errorToString (error: JsonDecode_Error) : StringRope =
    StringRope.fromString (JsonDecode_errorToStringHelp error [])


[<Struct>]
type Regex_Options =
    { CaseInsensitive: bool
      Multiline: bool }

[<Struct>]
type Regex_Match =
    { Match: StringRope
      Index: int64
      Number: int64
      Submatches: List<ValueOption<StringRope>> }

let Regex_fromString
    (string: StringRope)
    : ValueOption<System.Text.RegularExpressions.Regex> =
    try
        ValueSome(
            System.Text.RegularExpressions.Regex(
                StringRope.toString string,
                System.Text.RegularExpressions.RegexOptions.ECMAScript
            )
        )
    with _ ->
        ValueNone

let Regex_fromStringWith
    (options: Regex_Options)
    (string: StringRope)
    : ValueOption<System.Text.RegularExpressions.Regex> =
    try
        ValueSome(
            System.Text.RegularExpressions.Regex(
                StringRope.toString string,
                if options.Multiline then
                    if options.CaseInsensitive then
                        System.Text.RegularExpressions.RegexOptions.ECMAScript
                        ||| System.Text.RegularExpressions.RegexOptions.Multiline
                        ||| System.Text.RegularExpressions.RegexOptions.IgnoreCase
                    else
                        System.Text.RegularExpressions.RegexOptions.ECMAScript
                        ||| System.Text.RegularExpressions.RegexOptions.Multiline

                else if
                    // !options.Multiline
                    options.CaseInsensitive
                then
                    System.Text.RegularExpressions.RegexOptions.ECMAScript
                    ||| System.Text.RegularExpressions.RegexOptions.Singleline
                    ||| System.Text.RegularExpressions.RegexOptions.IgnoreCase
                else
                    System.Text.RegularExpressions.RegexOptions.ECMAScript
                    ||| System.Text.RegularExpressions.RegexOptions.Singleline
            )
        )
    with _ ->
        ValueNone

let Regex_never: System.Text.RegularExpressions.Regex =
    System.Text.RegularExpressions.Regex "/.^/"

let inline Regex_contains
    (regex: System.Text.RegularExpressions.Regex)
    (string: StringRope)
    : bool =
    regex.IsMatch(StringRope.toString string)

let inline Regex_split
    (regex: System.Text.RegularExpressions.Regex)
    (string: StringRope)
    : List<StringRope> =
    // can be optimized
    Array.toList (
        Array.map StringRope.fromString (regex.Split(StringRope.toString string))
    )

let inline Regex_splitAtMost
    (maxSplitCount: int64)
    (regex: System.Text.RegularExpressions.Regex)
    (string: StringRope)
    : List<StringRope> =
    // can be optimized
    Array.toList (
        Array.map
            StringRope.fromString
            (regex.Split(StringRope.toString string, int maxSplitCount))
    )

let inline regexMatchToRegex_MatchAtIndex0Based
    (matchNumber0Based: int64)
    (regexMatch: System.Text.RegularExpressions.Match)
    : Regex_Match =
    { Match = StringRope.fromString regexMatch.Value
      Index = regexMatch.Index
      Number = matchNumber0Based + 1L
      Submatches =
        Seq.toList (
            Seq.map
                (fun (subMatch: System.Text.RegularExpressions.Group) ->
                    // TODO when does elm return ValueNone?
                    ValueSome(StringRope.fromString subMatch.Value))
                regexMatch.Groups
        ) }

let inline Regex_find
    (regex: System.Text.RegularExpressions.Regex)
    (string: StringRope)
    : List<Regex_Match> =
    Seq.toList (
        Seq.mapi
            (fun index regexMatch ->
                regexMatchToRegex_MatchAtIndex0Based index regexMatch)
            (regex.Matches(StringRope.toString string))
    )

let inline Regex_findAtMost
    (maxMatchCount: int64)
    (regex: System.Text.RegularExpressions.Regex)
    (string: StringRope)
    : List<Regex_Match> =
    Seq.toList (
        Seq.mapi
            (fun index regexMatch ->
                regexMatchToRegex_MatchAtIndex0Based index regexMatch)
            (regex.Matches(StringRope.toString string, int maxMatchCount))
    )

let inline createRegexMatchNumber0BasedMap
    (regex: System.Text.RegularExpressions.Regex)
    (string: string)
    : Map<string, int64> =
    (Seq.fold
        (fun
            (soFar:
                {| Index: int64
                   Map: Map<string, int64> |})
            (regexMatch: System.Text.RegularExpressions.Match) ->
            {| Index = soFar.Index + 1L
               Map = Map.add regexMatch.Value soFar.Index soFar.Map |})
        {| Index = 0L; Map = Map.empty |}
        (regex.Matches string))
        .Map

let Regex_replace
    (regex: System.Text.RegularExpressions.Regex)
    (replacementForMatch: Regex_Match -> StringRope)
    (stringRope: StringRope)
    : StringRope =
    let string: string = StringRope.toString stringRope

    let matchNumbers0Based: Map<string, int64> =
        createRegexMatchNumber0BasedMap regex string

    StringRope.fromString (
        regex.Replace(
            string,
            System.Text.RegularExpressions.MatchEvaluator(fun regexMatch ->
                StringRope.toString (
                    replacementForMatch (
                        regexMatchToRegex_MatchAtIndex0Based
                            (Map.find regexMatch.Value matchNumbers0Based)
                            regexMatch
                    )
                ))
        )
    )

let Regex_replaceAtMost
    (maxMatchReplacementCount: int64)
    (regex: System.Text.RegularExpressions.Regex)
    (replacementForMatch: Regex_Match -> StringRope)
    (stringRope: StringRope)
    : StringRope =
    let string: string = StringRope.toString stringRope

    let matchNumbers0Based: Map<string, int64> =
        createRegexMatchNumber0BasedMap regex string

    StringRope.fromString (
        regex.Replace(
            string,
            System.Text.RegularExpressions.MatchEvaluator(fun regexMatch ->
                StringRope.toString (
                    replacementForMatch (
                        regexMatchToRegex_MatchAtIndex0Based
                            (Map.find regexMatch.Value matchNumbers0Based)
                            regexMatch
                    )
                )),
            int maxMatchReplacementCount
        )
    )


let inline Debug_log (tag: StringRope) (value: 'value) : 'value =
    System.Diagnostics.Debug.Print(StringRope.toString tag + ": {0}", value)

    value

let inline Debug_toString (value: 'value) : StringRope =
    StringRope.fromString (value.ToString())

let inline Debug_todo (message: string) : 'value =
    raise (new System.NotImplementedException(message))


let ElmKernelParser_isSubString
    (smallStringRope: StringRope)
    (offsetOriginal: int64)
    (rowOriginal: int64)
    (colOriginal: int64)
    (bigStringRope: StringRope)
    : struct (int64 * int64 * int64) =
    let smallString: string = StringRope.toString smallStringRope
    let bigString: string = StringRope.toString bigStringRope
    let smallLength: int = String.length smallString
    let mutable row: int = int rowOriginal
    let mutable col: int = int colOriginal
    let mutable offset: int = int offsetOriginal
    let mutable isGood: bool = int offset + smallLength <= String.length bigString
    let mutable i: int = 0

    while isGood && i < smallLength do
        let code = int (bigString[offset])
        isGood <- smallString[i] = bigString[offset]

        if code = 0x000A then // \n
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

    (struct ((if isGood then offset else -1), row, col))

let ElmKernelParser_isSubChar
    (predicate: int -> bool)
    (offset: int64)
    (stringRope: StringRope)
    : int64 =
    let string = StringRope.toString stringRope
    let offsetInt: int = int offset

    if String.length string <= offsetInt then
        -1
    else if System.Char.IsSurrogate(string[offsetInt]) then
        (if
             predicate (
                 System.Char.ConvertToUtf32(
                     string[offsetInt],
                     string[offsetInt + 1]
                 )
             )
         then
             offset + 2L
         else
             -1L)
    else if predicate (int (string[offsetInt])) then
        (if (string[offsetInt] = '\n') then -2L else offset + 1L)
    else
        -1

let inline ElmKernelParser_isAsciiCode
    (code: int64)
    (offset: int64)
    (string: StringRope)
    : bool =
    int64 ((StringRope.toString string)[int offset]) = code

let ElmKernelParser_chompBase10
    (offsetOriginal: int64)
    (stringRope: StringRope)
    : int64 =
    let string = StringRope.toString stringRope
    let mutable offset = int offsetOriginal
    let mutable foundNonBase10 = false

    while (offset < String.length string) && not foundNonBase10 do
        foundNonBase10 <- not (System.Char.IsAsciiDigit(string[offset]))
        offset <- offset + 1

    offset

let ElmKernelParser_consumeBase
    (base_: int64)
    (offsetOriginal: int64)
    (stringRope: StringRope)
    : struct (int64 * int64) =
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

    (struct (offset, total))

let ElmKernelParser_consumeBase16
    (offsetOriginal: int64)
    (stringRope: StringRope)
    : struct (int64 * int64) =
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

    (struct (offset, total))

let ElmKernelParser_findSubString
    (smallStringRope: StringRope)
    (offsetOriginal: int64)
    (rowOriginal: int64)
    (colOriginal: int64)
    (bigStringRope: StringRope)
    : struct (int64 * int64 * int64) =
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
        let code = int (bigString[offset])
        offset <- offset + 1

        if code = 0x000A then // \n
            col <- 1
            row <- row + 1
        else
            col <- col + 1

            if (code &&& 0xF800) = 0xD800 then
                offset <- offset + 1

    (struct (newOffset, row, col))

let inline ElmKernelUrl_percentEncode (string: StringRope) : StringRope =
    StringRope.fromString (
        System.Net.WebUtility.UrlEncode(StringRope.toString string)
    )

let inline ElmKernelUrl_percentDecode
    (string: StringRope)
    : ValueOption<StringRope> =
    match System.Net.WebUtility.UrlDecode(StringRope.toString string) with
    | null -> ValueNone
    | decodedString -> ValueSome(StringRope.fromString decodedString)


[<Struct>]
type Random_Seed =
    | Random_Seed of
        Random_Seed:
            // can be optimized by switching to int32
            (struct (int64 * int64))

type Random_Generator<'a> = Random_Seed -> (struct ('a * Random_Seed))

let Random_step (generator: Random_Generator<'a>) (seed: Random_Seed) =
    generator seed

let Random_peel (Random_Seed(state, _): Random_Seed) =
    let word: int64 =
        (*)
            ((^^^)
                state
                (Bitwise_shiftRightZfBy
                    ((+) (Bitwise_shiftRightZfBy 28L state) 4L)
                    state))
            277803737L

    Bitwise_shiftRightZfBy 0L ((^^^) (Bitwise_shiftRightZfBy 22L word) word)

let Random_next (Random_Seed(state0, incr): Random_Seed) =
    Random_Seed(
        struct (Bitwise_shiftRightZfBy 0L ((+) ((*) state0 1664525L) incr), incr)
    )

let Random_minInt: int64 = -2147483648L

let Random_maxInt: int64 = 2147483647L

let Random_map5
    (func: 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
    (genA: Random_Generator<'a>)
    (genB: Random_Generator<'b>)
    (genC: Random_Generator<'c>)
    (genD: Random_Generator<'d>)
    (genE: Random_Generator<'e>)
    =
    fun (seed0: Random_Seed) ->
        let ((struct (a, seed1)): (struct ('a * Random_Seed))) = genA seed0

        let ((struct (b, seed2)): (struct ('b * Random_Seed))) = genB seed1

        let ((struct (c, seed3)): (struct ('c * Random_Seed))) = genC seed2

        let ((struct (d, seed4)): (struct ('d * Random_Seed))) = genD seed3

        let ((struct (e, seed5)): (struct ('e * Random_Seed))) = genE seed4

        (struct (func a b c d e, seed5))

let Random_map4
    (func: 'a -> 'b -> 'c -> 'd -> 'e)
    (genA: Random_Generator<'a>)
    (genB: Random_Generator<'b>)
    (genC: Random_Generator<'c>)
    (genD: Random_Generator<'d>)
    =
    fun (seed0: Random_Seed) ->
        let ((struct (a, seed1)): (struct ('a * Random_Seed))) = genA seed0

        let ((struct (b, seed2)): (struct ('b * Random_Seed))) = genB seed1

        let ((struct (c, seed3)): (struct ('c * Random_Seed))) = genC seed2

        let ((struct (d, seed4)): (struct ('d * Random_Seed))) = genD seed3

        (struct (func a b c d, seed4))

let Random_map3
    (func: 'a -> 'b -> 'c -> 'd)
    (genA: Random_Generator<'a>)
    (genB: Random_Generator<'b>)
    (genC: Random_Generator<'c>)
    =
    fun (seed0: Random_Seed) ->
        let ((struct (a, seed1)): (struct ('a * Random_Seed))) = genA seed0

        let ((struct (b, seed2)): (struct ('b * Random_Seed))) = genB seed1

        let ((struct (c, seed3)): (struct ('c * Random_Seed))) = genC seed2

        (struct (func a b c, seed3))

let Random_map2
    (func: 'a -> 'b -> 'c)
    (genA: Random_Generator<'a>)
    (genB: Random_Generator<'b>)
    =
    fun (seed0: Random_Seed) ->
        let ((struct (a, seed1)): (struct ('a * Random_Seed))) = genA seed0

        let ((struct (b, seed2)): (struct ('b * Random_Seed))) = genB seed1

        (struct (func a b, seed2))

let Random_pair (genA: Random_Generator<'a>) (genB: Random_Generator<'b>) =
    Random_map2 (fun (a: 'a) (b: 'b) -> (struct (a, b))) genA genB

let Random_map (func: 'a -> 'b) (genA: Random_Generator<'a>) =
    fun (seed0: Random_Seed) ->
        let ((struct (a, seed1)): (struct ('a * Random_Seed))) = genA seed0

        (struct (func a, seed1))

let rec Random_listHelp
    (revList: List<'a>)
    (n: int64)
    (gen: Random_Seed -> (struct ('a * Random_Seed)))
    (seed: Random_Seed)
    =
    if (<) n 1L then
        (struct (revList, seed))

    else
        let ((struct (value, newSeed)): (struct ('a * Random_Seed))) = gen seed

        Random_listHelp (List_cons value revList) ((-) n 1L) gen newSeed

let Random_list (n: int64) (gen: Random_Generator<'a>) =
    fun (seed: Random_Seed) -> Random_listHelp [] n gen seed

let Random_lazy (callback: unit -> Random_Generator<'a>) =
    fun (seed: Random_Seed) ->
        let (gen: Random_Generator<'a>) = callback ()

        gen seed

let Random_int (a: int64) (b: int64) =
    fun (seed0: Random_Seed) ->
        let ((struct (lo, hi)): (struct (int64 * int64))) =
            if (<) a b then
                (struct (a, b))

            else
                (struct (b, a))

        let range: int64 = (+) ((-) hi lo) 1L

        if Basics_eq ((&&&) ((-) range 1L) range) 0L then
            (struct ((+)
                         (Bitwise_shiftRightZfBy
                             0L
                             ((&&&) ((-) range 1L) (Random_peel seed0)))
                         lo,
                     Random_next seed0))

        else
            let threshold: int64 =
                Bitwise_shiftRightZfBy
                    0L
                    (Basics_remainderBy
                        range
                        (Bitwise_shiftRightZfBy 0L (Basics_inegate range)))

            let rec accountForBias (seed: Random_Seed) =
                let x: int64 = Random_peel seed

                let seedN: Random_Seed = Random_next seed

                if (<) x threshold then
                    accountForBias seedN

                else
                    (struct ((+) (Basics_remainderBy range x) lo, seedN))

            accountForBias seed0

let Random_initialSeed (x: int64) =
    let (Random_Seed(state1, incr): Random_Seed) =
        Random_next(Random_Seed(struct (0L, 1013904223L)))

    let state2: int64 = Bitwise_shiftRightZfBy 0L ((+) state1 x)

    Random_next(Random_Seed(struct (state2, incr)))

let Random_independentSeed: Random_Generator<Random_Seed> =
    fun (seed0: Random_Seed) ->
        let makeIndependentSeed (state: int64) (b: int64) (c: int64) =
            Random_next(
                Random_Seed(
                    struct (state, Bitwise_shiftRightZfBy 0L ((|||) 1L ((^^^) b c)))
                )
            )

        let gen: Random_Generator<int64> = Random_int 0L 4294967295L

        Random_step (Random_map3 makeIndependentSeed gen gen gen) seed0

let rec Random_getByWeight
    ((struct (weight, value)): (struct (float * 'a)))
    (others: List<(struct (float * 'a))>)
    (countdown: float)
    =
    match others with
    | [] -> value

    | second :: otherOthers ->
        if (<=) countdown (System.Double.Abs weight) then
            value

        else
            Random_getByWeight
                second
                otherOthers
                ((-) countdown (System.Double.Abs weight))

let Random_float (a: float) (b: float) =
    fun (seed0: Random_Seed) ->
        let seed1: Random_Seed = Random_next seed0

        let range: float = System.Double.Abs((-) b a)

        let n1: int64 = Random_peel seed1

        let n0: int64 = Random_peel seed0

        let lo: float = (*) (float ((&&&) 134217727L n1)) 1.0

        let hi: float = (*) (float ((&&&) 67108863L n0)) 1.0

        let val_: float = (/) ((+) ((*) hi 134217728.0) lo) 9007199254740992.0

        let scaled: float = (+) ((*) val_ range) a

        struct (scaled, Random_next seed1)

let Random_weighted
    (first: (struct (float * 'a)))
    (others: List<(struct (float * 'a))>)
    =
    let normalize ((struct (weight, _)): (struct (float * 'ignored))) =
        System.Double.Abs weight

    let total: float = (+) (normalize first) (List.sum (List.map normalize others))

    Random_map (Random_getByWeight first others) (Random_float 0.0 total)

let inline Random_constant (value: 'a) : Random_Generator<'a> =
    fun (seed: Random_Seed) -> struct (value, seed)

let Random_andThen
    (callback: 'a -> Random_Generator<'b>)
    (genA: Random_Generator<'a>)
    =
    fun (seed: Random_Seed) ->
        let (struct (result, newSeed)): (struct ('a * Random_Seed)) = genA seed

        let genB: Random_Generator<'b> = callback result

        genB newSeed

let Random_addOne (value: 'a) = (struct (1.0, value))

let Random_uniform (value: 'a) (valueList: List<'a>) =
    Random_weighted (struct (1.0, value)) (List.map Random_addOne valueList)


[<Struct>]
type Time_Posix = Time_Posix of int64

[<Struct>]
type Time_Era = { Start: int64; Offset: int64 }

[<Struct>]
type Time_Zone = Time_Zone of (struct (int64 * List<Time_Era>))

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
    | Time_Name of Time_Name: StringRope
    | Time_Offset of Time_Offset: int64

[<Struct>]
type Time_Civil =
    { Year: int64
      Month: int64
      Day: int64 }

let inline Time_posixToMillis ((Time_Posix millis): Time_Posix) : int64 = millis
let inline Time_millisToPosix (millis: int64) : Time_Posix = Time_Posix millis

let Time_utc: Time_Zone = Time_Zone(struct (0L, []))

let Time_customZone (n: int64) (eras: List<Time_Era>) : Time_Zone =
    Time_Zone(struct (n, eras))

let inline flooredDiv (numerator: int64) (denominator: int64) : int64 =
    int64 (floor (float numerator / float denominator))

let rec Time_toAdjustedMinutesHelp
    (defaultOffset: int64)
    (posixMinutes: int64)
    (eras: List<Time_Era>)
    : int64 =
    match eras with
    | [] -> posixMinutes + defaultOffset

    | era :: olderEras ->
        if era.Start < posixMinutes then
            posixMinutes + era.Offset
        else
            Time_toAdjustedMinutesHelp defaultOffset posixMinutes olderEras

let Time_toAdjustedMinutes
    ((Time_Zone(struct (defaultOffset, eras))): Time_Zone)
    (time: Time_Posix)
    =
    Time_toAdjustedMinutesHelp
        defaultOffset
        (flooredDiv (Time_posixToMillis time) 60000)
        eras

let Time_toCivil (minutes: int64) : Time_Civil =
    let rawDay = flooredDiv minutes (60L * 24L) + 719468L
    let era = (if rawDay >= 0 then rawDay else rawDay - 146096L) / 146097L
    let dayOfEra = rawDay - era * 146097L // [0, 146096]

    let yearOfEra =
        (dayOfEra - dayOfEra / 1460L + dayOfEra / 36524L - dayOfEra / 146096L)
        / 365L // [0, 399]

    let year = yearOfEra + era * 400L

    let dayOfYear =
        dayOfEra - (365L * yearOfEra + yearOfEra / 4L - yearOfEra / 100L) // [0, 365]

    let mp = (5L * dayOfYear + 2L) / 153L // [0, 11]
    let month = mp + (if mp < 10 then 3L else -9L) // [1, 12]

    { Year = year + (if month <= 2 then 1L else 0L)
      Month = month
      Day = dayOfYear - (153L * mp + 2L) / 5L + 1L // [1, 31]
    }

let Time_toYear (zone: Time_Zone) (time: Time_Posix) : int64 =
    (Time_toCivil(Time_toAdjustedMinutes zone time)).Year

let Time_toMonth (zone: Time_Zone) (time: Time_Posix) : Time_Month =
    match (Time_toCivil(Time_toAdjustedMinutes zone time)).Month with
    | 1L -> Time_Jan
    | 2L -> Time_Feb
    | 3L -> Time_Mar
    | 4L -> Time_Apr
    | 5L -> Time_May
    | 6L -> Time_Jun
    | 7L -> Time_Jul
    | 8L -> Time_Aug
    | 9L -> Time_Sep
    | 10L -> Time_Oct
    | 11L -> Time_Nov
    | _ -> Time_Dec

let Time_toDay (zone: Time_Zone) (time: Time_Posix) : int64 =
    (Time_toCivil(Time_toAdjustedMinutes zone time)).Day

let Time_toWeekday (zone: Time_Zone) (time: Time_Posix) : Time_Weekday =
    match
        Basics_modBy 7 (flooredDiv (Time_toAdjustedMinutes zone time) (60L * 24L))
    with
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


type Bytes_Bytes = array<byte>

[<Struct>]
type Bytes_Endianness =
    | Bytes_LE
    | Bytes_BE

type BytesEncode_Encoder =
    | BytesEncode_I8 of int64
    | BytesEncode_I16 of (struct (Bytes_Endianness * int64))
    | BytesEncode_I32 of (struct (Bytes_Endianness * int64))
    | BytesEncode_U8 of int64
    | BytesEncode_U16 of (struct (Bytes_Endianness * int64))
    | BytesEncode_U32 of (struct (Bytes_Endianness * int64))
    | BytesEncode_F32 of (struct (Bytes_Endianness * float))
    | BytesEncode_F64 of (struct (Bytes_Endianness * float))
    | BytesEncode_Seq of (struct (int * List<BytesEncode_Encoder>))
    | BytesEncode_Utf8 of (struct (int * StringRope))
    | BytesEncode_Bytes of Bytes_Bytes

let Bytes_width (bytes: Bytes_Bytes) : int64 = Array.length bytes


let BytesEncode_getStringWidth (string: StringRope) : int64 =
    System.Text.Encoding.UTF8.GetByteCount(StringRope.toString string)

let BytesEncode_signedInt8 (i8: int64) : BytesEncode_Encoder = BytesEncode_I8 i8

let BytesEncode_signedInt16
    (endianness: Bytes_Endianness)
    (i16: int64)
    : BytesEncode_Encoder =
    BytesEncode_I16(endianness, i16)

let BytesEncode_signedInt32
    (endianness: Bytes_Endianness)
    (i32: int64)
    : BytesEncode_Encoder =
    BytesEncode_I32(endianness, i32)

let BytesEncode_unsignedInt8 (u8: int64) : BytesEncode_Encoder = BytesEncode_U8 u8

let BytesEncode_unsignedInt16
    (endianness: Bytes_Endianness)
    (u16: int64)
    : BytesEncode_Encoder =
    BytesEncode_U16(endianness, u16)

let BytesEncode_unsignedInt32
    (endianness: Bytes_Endianness)
    (u32: int64)
    : BytesEncode_Encoder =
    BytesEncode_U32(endianness, u32)

let BytesEncode_float32
    (endianness: Bytes_Endianness)
    (f32: float)
    : BytesEncode_Encoder =
    BytesEncode_F32(endianness, f32)

let BytesEncode_float64
    (endianness: Bytes_Endianness)
    (f64: float)
    : BytesEncode_Encoder =
    BytesEncode_F64(endianness, f64)

let BytesEncode_bytes (bytes: Bytes_Bytes) : BytesEncode_Encoder =
    BytesEncode_Bytes bytes

let BytesEncode_string (string: StringRope) : BytesEncode_Encoder =
    BytesEncode_Utf8(
        System.Text.Encoding.UTF8.GetByteCount(StringRope.toString string),
        string
    )

let BytesEncode_EncoderByteCount (encoder: BytesEncode_Encoder) : int32 =
    match encoder with
    | BytesEncode_I8 _ -> 1
    | BytesEncode_I16 _ -> 2
    | BytesEncode_I32 _ -> 4
    | BytesEncode_U8 _ -> 1
    | BytesEncode_U16 _ -> 2
    | BytesEncode_U32 _ -> 4
    | BytesEncode_F32 _ -> 4
    | BytesEncode_F64 _ -> 8
    | BytesEncode_Seq(w, _) -> w
    | BytesEncode_Utf8(w, _) -> w
    | BytesEncode_Bytes bytes -> Array.length bytes

let BytesEncode_sequence
    (encoders: List<BytesEncode_Encoder>)
    : BytesEncode_Encoder =
    BytesEncode_Seq(
        Seq.sum (Seq.map BytesEncode_EncoderByteCount encoders),
        encoders
    )

let convertedBytesAdaptEndianness
    (endianness: Bytes_Endianness)
    (asLeBytes: Bytes_Bytes)
    : array<byte> =
    // can be optimized to use specialized endian operations
    // https://learn.microsoft.com/en-us/dotnet/api/system.buffers.binary.binaryprimitives?view=net-9.0
    if (endianness = Bytes_LE) <> System.BitConverter.IsLittleEndian then
        System.Array.Reverse(asLeBytes)

    asLeBytes

let BytesEncode_encode (encoder: BytesEncode_Encoder) : Bytes_Bytes =
    let mutableBuffer =
        new System.IO.MemoryStream(BytesEncode_EncoderByteCount encoder)

    let mutable toEncodeNext = encoder

    let mutable mutableRemainingRightEncoders =
        System.Collections.Generic.Stack<BytesEncode_Encoder>()

    let mutable shouldKeepGoing = true

    while shouldKeepGoing do
        match toEncodeNext with
        | BytesEncode_I8 i8 ->
            mutableBuffer.WriteByte(byte (sbyte i8))

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_I16(endianness, i16) ->
            mutableBuffer.Write(
                convertedBytesAdaptEndianness
                    endianness
                    (System.BitConverter.GetBytes(int16 i16))
            )

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_I32(endianness, i32) ->
            mutableBuffer.Write(
                convertedBytesAdaptEndianness
                    endianness
                    (System.BitConverter.GetBytes(int32 i32))
            )

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_U8 u8 ->
            mutableBuffer.WriteByte(byte u8)

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_U16(endianness, u16) ->
            mutableBuffer.Write(
                convertedBytesAdaptEndianness
                    endianness
                    (System.BitConverter.GetBytes(uint16 u16))
            )

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_U32(endianness, u32) ->
            mutableBuffer.Write(
                convertedBytesAdaptEndianness
                    endianness
                    (System.BitConverter.GetBytes(uint32 u32))
            )

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_F32(endianness, f32) ->
            mutableBuffer.Write(
                convertedBytesAdaptEndianness
                    endianness
                    (System.BitConverter.GetBytes(float32 f32))
            )

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_F64(endianness, f64) ->
            mutableBuffer.Write(
                convertedBytesAdaptEndianness
                    endianness
                    (System.BitConverter.GetBytes f64)
            )

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_Utf8(_byteLength, stringRope) ->
            mutableBuffer.Write(
                new System.ReadOnlySpan<byte>(
                    System.Text.Encoding.UTF8.GetBytes(
                        StringRope.toString stringRope
                    )
                )
            )

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_Bytes byteArray ->
            mutableBuffer.Write(new System.ReadOnlySpan<byte>(byteArray))

            if mutableRemainingRightEncoders.Count = 0 then
                shouldKeepGoing <- false
            else
                toEncodeNext <- mutableRemainingRightEncoders.Pop()
        | BytesEncode_Seq(_byteLength, encoders) ->
            match encoders with
            | [] ->
                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | nextSubEncoder :: subEncodersAfterNextSubEncoder ->
                toEncodeNext <- nextSubEncoder
                // can probably be optimized
                for subEncoderAfterNextSubEncoder in
                    Seq.rev subEncodersAfterNextSubEncoder do
                    mutableRemainingRightEncoders.Push subEncoderAfterNextSubEncoder

    mutableBuffer.ToArray()


type BytesDecode_Decoder<'value> =
    Bytes_Bytes -> int32 -> ValueOption<struct (int32 * 'value)>

[<Struct>]
type BytesDecode_Step<'state, 'a> =
    | BytesDecode_Loop of BytesDecode_Loop: 'state
    | BytesDecode_Done of BytesDecode_Done: 'a

let BytesDecode_decode
    (decoder: BytesDecode_Decoder<'value>)
    (bytes: Bytes_Bytes)
    : ValueOption<'value> =
    match decoder bytes 0 with
    | ValueNone -> ValueNone
    | ValueSome(_, value) -> ValueSome value

let BytesDecode_succeed (value: 'value) : BytesDecode_Decoder<'value> =
    fun _bytes index -> ValueSome(index, value)

let BytesDecode_fail: BytesDecode_Decoder<'value> = fun _bytes _index -> ValueNone

let BytesDecode_andThen
    (valueToFollowingDecoder: 'value -> BytesDecode_Decoder<'mappedValue>)
    (decoder: BytesDecode_Decoder<'value>)
    : BytesDecode_Decoder<'mappedValue> =
    fun bytes index ->
        match decoder bytes index with
        | ValueNone -> ValueNone
        | ValueSome(indexAfter, value) ->
            valueToFollowingDecoder value bytes indexAfter

let BytesDecode_map
    (valueChange: 'value -> 'mappedValue)
    (decoder: BytesDecode_Decoder<'value>)
    : BytesDecode_Decoder<'mappedValue> =
    fun bytes index ->
        match decoder bytes index with
        | ValueNone -> ValueNone
        | ValueSome(indexAfter, value) -> ValueSome(indexAfter, valueChange value)

let rec BytesDecode_loop
    (initialState: 'state)
    (step: 'state -> BytesDecode_Decoder<BytesDecode_Step<'state, 'a>>)
    : BytesDecode_Decoder<'a> =
    fun bytes index ->
        match step initialState bytes index with
        | ValueNone -> ValueNone
        | ValueSome(indexAfterStep, stepValue) ->
            match stepValue with
            | BytesDecode_Loop newState ->
                BytesDecode_loop newState step bytes indexAfterStep
            | BytesDecode_Done result -> ValueSome(indexAfterStep, result)

let BytesDecode_map2
    (valuesCombine: 'a -> 'b -> 'combined)
    (aDecoder: BytesDecode_Decoder<'a>)
    (bDecoder: BytesDecode_Decoder<'b>)
    : BytesDecode_Decoder<'combined> =
    fun bytes index ->
        match aDecoder bytes index with
        | ValueNone -> ValueNone
        | ValueSome(indexAfterA, a) ->
            match bDecoder bytes indexAfterA with
            | ValueNone -> ValueNone
            | ValueSome(indexAfterB, b) -> ValueSome(indexAfterB, valuesCombine a b)

let BytesDecode_map3
    (valuesCombine: 'a -> 'b -> 'c -> 'combined)
    (aDecoder: BytesDecode_Decoder<'a>)
    (bDecoder: BytesDecode_Decoder<'b>)
    (cDecoder: BytesDecode_Decoder<'c>)
    : BytesDecode_Decoder<'combined> =
    fun bytes index ->
        match aDecoder bytes index with
        | ValueNone -> ValueNone
        | ValueSome(indexAfterA, a) ->
            match bDecoder bytes indexAfterA with
            | ValueNone -> ValueNone
            | ValueSome(indexAfterB, b) ->
                match cDecoder bytes indexAfterB with
                | ValueNone -> ValueNone
                | ValueSome(indexAfterC, c) ->
                    ValueSome(indexAfterC, valuesCombine a b c)

let BytesDecode_map4
    (valuesCombine: 'a -> 'b -> 'c -> 'd -> 'combined)
    (aDecoder: BytesDecode_Decoder<'a>)
    (bDecoder: BytesDecode_Decoder<'b>)
    (cDecoder: BytesDecode_Decoder<'c>)
    (dDecoder: BytesDecode_Decoder<'d>)
    : BytesDecode_Decoder<'combined> =
    fun bytes index ->
        match aDecoder bytes index with
        | ValueNone -> ValueNone
        | ValueSome(indexAfterA, a) ->
            match bDecoder bytes indexAfterA with
            | ValueNone -> ValueNone
            | ValueSome(indexAfterB, b) ->
                match cDecoder bytes indexAfterB with
                | ValueNone -> ValueNone
                | ValueSome(indexAfterC, c) ->
                    match dDecoder bytes indexAfterC with
                    | ValueNone -> ValueNone
                    | ValueSome(indexAfterD, d) ->
                        ValueSome(indexAfterD, valuesCombine a b c d)

let BytesDecode_map5
    (valuesCombine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined)
    (aDecoder: BytesDecode_Decoder<'a>)
    (bDecoder: BytesDecode_Decoder<'b>)
    (cDecoder: BytesDecode_Decoder<'c>)
    (dDecoder: BytesDecode_Decoder<'d>)
    (eDecoder: BytesDecode_Decoder<'e>)
    : BytesDecode_Decoder<'combined> =
    fun bytes index ->
        match aDecoder bytes index with
        | ValueNone -> ValueNone
        | ValueSome(indexAfterA, a) ->
            match bDecoder bytes indexAfterA with
            | ValueNone -> ValueNone
            | ValueSome(indexAfterB, b) ->
                match cDecoder bytes indexAfterB with
                | ValueNone -> ValueNone
                | ValueSome(indexAfterC, c) ->
                    match dDecoder bytes indexAfterC with
                    | ValueNone -> ValueNone
                    | ValueSome(indexAfterD, d) ->
                        match eDecoder bytes indexAfterD with
                        | ValueNone -> ValueNone
                        | ValueSome(indexAfterE, e) ->
                            ValueSome(indexAfterE, valuesCombine a b c d e)

let BytesDecode_signedInt8: BytesDecode_Decoder<int64> =
    fun bytes index ->
        let indexAfter: int = index + 1

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(indexAfter, int64 (sbyte (bytes[index])))

let BytesDecode_signedInt16
    (endianness: Bytes_Endianness)
    : BytesDecode_Decoder<int64> =
    fun bytes index ->
        let indexAfter: int = index + 2

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(
                indexAfter,
                int64 (
                    match endianness with
                    | Bytes_LE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadInt16LittleEndian(
                            bytes[index..indexAfter]
                        )
                    | Bytes_BE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadInt16BigEndian(
                            bytes[index..indexAfter]
                        )
                )
            )

let BytesDecode_signedInt32
    (endianness: Bytes_Endianness)
    : BytesDecode_Decoder<int64> =
    fun bytes index ->
        let indexAfter: int = index + 4

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(
                indexAfter,
                int64 (
                    match endianness with
                    | Bytes_LE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadInt32LittleEndian(
                            bytes[index..indexAfter]
                        )
                    | Bytes_BE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(
                            bytes[index..indexAfter]
                        )
                )
            )

let BytesDecode_unsignedInt8: BytesDecode_Decoder<int64> =
    fun bytes index ->
        let indexAfter: int = index + 1

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(indexAfter, int64 (bytes[index]))

let BytesDecode_unsignedInt16
    (endianness: Bytes_Endianness)
    : BytesDecode_Decoder<int64> =
    fun bytes index ->
        let indexAfter: int = index + 2

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(
                indexAfter,
                int64 (
                    match endianness with
                    | Bytes_LE ->
                        System
                            .Buffers
                            .Binary
                            .BinaryPrimitives
                            .ReadUInt16LittleEndian(bytes[index..indexAfter])
                    | Bytes_BE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadUInt16BigEndian(
                            bytes[index..indexAfter]
                        )
                )
            )

let BytesDecode_unsignedInt32
    (endianness: Bytes_Endianness)
    : BytesDecode_Decoder<int64> =
    fun bytes index ->
        let indexAfter: int = index + 4

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(
                indexAfter,
                int64 (
                    match endianness with
                    | Bytes_LE ->
                        System
                            .Buffers
                            .Binary
                            .BinaryPrimitives
                            .ReadUInt32LittleEndian(bytes[index..indexAfter])
                    | Bytes_BE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(
                            bytes[index..indexAfter]
                        )
                )
            )

let BytesDecode_float32
    (endianness: Bytes_Endianness)
    : BytesDecode_Decoder<float> =
    fun bytes index ->
        let indexAfter: int = index + 4

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(
                indexAfter,
                float (
                    match endianness with
                    | Bytes_LE ->
                        System
                            .Buffers
                            .Binary
                            .BinaryPrimitives
                            .ReadSingleLittleEndian(bytes[index..indexAfter])
                    | Bytes_BE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadSingleBigEndian(
                            bytes[index..indexAfter]
                        )
                )
            )

let BytesDecode_float64
    (endianness: Bytes_Endianness)
    : BytesDecode_Decoder<float> =
    fun bytes index ->
        let indexAfter: int = index + 8

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(
                indexAfter,
                match endianness with
                | Bytes_LE ->
                    System.Buffers.Binary.BinaryPrimitives.ReadDoubleLittleEndian(
                        bytes[index..indexAfter]
                    )
                | Bytes_BE ->
                    System.Buffers.Binary.BinaryPrimitives.ReadDoubleBigEndian(
                        bytes[index..indexAfter]
                    )
            )

let BytesDecode_bytes (byteCountToRead: int64) : BytesDecode_Decoder<Bytes_Bytes> =
    fun bytes index ->
        let indexAfter: int = index + int byteCountToRead

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(indexAfter, bytes[index..indexAfter])

let BytesDecode_string (byteCountToRead: int64) : BytesDecode_Decoder<StringRope> =
    fun bytes index ->
        let indexAfter: int = index + 8

        if indexAfter >= Array.length bytes then
            ValueNone
        else
            ValueSome(
                indexAfter,
                StringRope.fromString (
                    System.Text.Encoding.UTF8.GetString(
                        bytes,
                        index,
                        int byteCountToRead
                    )
                )
            )


let VirtualDom_RE_js: System.Text.RegularExpressions.Regex =
    System.Text.RegularExpressions.Regex
        "/^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i"

let VirtualDom_RE_js_html: System.Text.RegularExpressions.Regex =
    System.Text.RegularExpressions.Regex
        "/^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i"

let VirtualDom_noJavaScriptUri (uri: StringRope) : StringRope =
    if VirtualDom_RE_js.IsMatch(StringRope.toString uri) then
        stringRopeEmpty
    else
        uri

let VirtualDom_noJavaScriptOrHtmlUri (uri: StringRope) : StringRope =
    if VirtualDom_RE_js_html.IsMatch(StringRope.toString uri) then
        stringRopeEmpty
    else
        uri

[<Struct>]
type VirtualDom_ModifierAttribute =
    { Namespace: ValueOption<string>
      Key: string
      Value: string }

[<Struct>]
type VirtualDom_ModifierStyle = { Key: string; Value: string }

[<Struct>]
type VirtualDom_ModifierProperty =
    { Key: string
      Value: System.Text.Json.Nodes.JsonNode }

[<Struct>]
type Generated_Message_PreventDefault_StopPropagation<'message, 'preventDefault, 'stopPropagation>
    =
    { Message: 'message
      PreventDefault: 'preventDefault
      StopPropagation: 'stopPropagation }

type VirtualDom_CustomHandledEvent<'event> =
    Generated_Message_PreventDefault_StopPropagation<'event, bool, bool>

[<Struct>]
type VirtualDom_Handler<'event> =
    | VirtualDom_Normal of VirtualDom_Normal: (JsonDecode_Decoder<'event>)
    | VirtualDom_MayStopPropagation of
        VirtualDom_MayStopPropagation: (JsonDecode_Decoder<struct ('event * bool)>)
    | VirtualDom_MayPreventDefault of
        VirtualDom_MayPreventDefault: (JsonDecode_Decoder<struct ('event * bool)>)
    | VirtualDom_Custom of
        VirtualDom_Custom:
            (JsonDecode_Decoder<VirtualDom_CustomHandledEvent<'event>>)

let inline VirtualDom_customHandledEventMap
    (eventChange: 'event -> 'eventMapped)
    (handledEvent: VirtualDom_CustomHandledEvent<'event>)
    : VirtualDom_CustomHandledEvent<'eventMapped> =
    { Message = eventChange handledEvent.Message
      StopPropagation = handledEvent.StopPropagation
      PreventDefault = handledEvent.PreventDefault }

let VirtualDom_handlerMap
    (eventChange: 'event -> 'eventMapped)
    (handler: VirtualDom_Handler<'event>)
    : VirtualDom_Handler<'eventMapped> =
    match handler with
    | VirtualDom_Normal decoder ->
        VirtualDom_Normal(JsonDecode_map eventChange decoder)
    | VirtualDom_MayStopPropagation decoder ->
        VirtualDom_MayStopPropagation(
            JsonDecode_map
                (fun (struct (event, stopPropagation)) ->
                    struct (eventChange event, stopPropagation))
                decoder
        )
    | VirtualDom_MayPreventDefault decoder ->
        VirtualDom_MayPreventDefault(
            JsonDecode_map
                (fun (struct (event, preventDefault)) ->
                    struct (eventChange event, preventDefault))
                decoder
        )
    | VirtualDom_Custom decoder ->
        VirtualDom_Custom(
            JsonDecode_map
                (fun custom -> VirtualDom_customHandledEventMap eventChange custom)
                decoder
        )

[<Struct>]
type VirtualDom_ModifierEventListener<'event> =
    { Name: string
      Handler: VirtualDom_Handler<'event> }

type VirtualDom_Attribute<'event> =
    | VirtualDom_ModifierAttribute of VirtualDom_ModifierAttribute
    | VirtualDom_ModifierStyle of VirtualDom_ModifierStyle
    | VirtualDom_ModifierProperty of VirtualDom_ModifierProperty
    | VirtualDom_ModifierEventListener of VirtualDom_ModifierEventListener<'event>

[<Struct>]
type VirtualDom_Element<'event> =
    { Tag: string
      Namespace: ValueOption<string>
      Subs: List<VirtualDom_Node<'event>>
      Modifiers: List<VirtualDom_Attribute<'event>> }

and [<Struct>] VirtualDom_ElementKeyed<'event> =
    { Tag: string
      Namespace: ValueOption<string>
      Subs: List<VirtualDom_SubNodeKeyed<'event>>
      Modifiers: List<VirtualDom_Attribute<'event>> }

and [<Struct>] VirtualDom_SubNodeKeyed<'event> =
    { Key: string
      Node: VirtualDom_Node<'event> }

and [<Struct>] VirtualDom_NodeLazy<'event> =
    { Keys: array<obj>
      Construct: unit -> VirtualDom_Node<'event> }

and VirtualDom_Node<'event> =
    | VirtualDom_Text of string
    | VirtualDom_Element of VirtualDom_Element<'event>
    | VirtualDom_ElementKeyed of VirtualDom_ElementKeyed<'event>
    | VirtualDom_NodeLazy of VirtualDom_NodeLazy<'event>

let inline VirtualDom_text (string: StringRope) : VirtualDom_Node<'event> =
    VirtualDom_Text(StringRope.toString string)

let inline VirtualDom_node
    (tag: StringRope)
    (modifiers: List<VirtualDom_Attribute<'event>>)
    (subs: List<VirtualDom_Node<'event>>)
    : VirtualDom_Node<'event> =
    VirtualDom_Element
        { Tag = StringRope.toString tag
          Namespace = ValueNone
          Modifiers = modifiers
          Subs = subs }

let inline VirtualDom_nodeNS
    (namespace_: StringRope)
    (tag: StringRope)
    (modifiers: List<VirtualDom_Attribute<'event>>)
    (subs: List<VirtualDom_Node<'event>>)
    : VirtualDom_Node<'event> =
    VirtualDom_Element
        { Tag = StringRope.toString tag
          Namespace = ValueSome(StringRope.toString namespace_)
          Modifiers = modifiers
          Subs = subs }

let inline VirtualDom_keyedNode
    (tag: StringRope)
    (modifiers: List<VirtualDom_Attribute<'event>>)
    (subs: List<struct (StringRope * VirtualDom_Node<'event>)>)
    : VirtualDom_Node<'event> =
    VirtualDom_ElementKeyed
        { Tag = StringRope.toString tag
          Namespace = ValueNone
          Modifiers = modifiers
          Subs =
            List.map
                (fun (struct (key, node)) ->
                    { Key = StringRope.toString key
                      Node = node })
                subs }

let inline VirtualDom_keyedNodeNS
    (namespace_: StringRope)
    (tag: StringRope)
    (modifiers: List<VirtualDom_Attribute<'event>>)
    (subs: List<struct (StringRope * VirtualDom_Node<'event>)>)
    : VirtualDom_Node<'event> =
    VirtualDom_ElementKeyed
        { Tag = StringRope.toString tag
          Namespace = ValueSome(StringRope.toString namespace_)
          Modifiers = modifiers
          Subs =
            List.map
                (fun (struct (key, node)) ->
                    { Key = StringRope.toString key
                      Node = node })
                subs }

let inline VirtualDom_style
    (key: StringRope)
    (value: StringRope)
    : VirtualDom_Attribute<'event> =
    VirtualDom_ModifierStyle
        { Key = StringRope.toString key
          Value = StringRope.toString value }

let inline VirtualDom_property
    (key: StringRope)
    (value: System.Text.Json.Nodes.JsonNode)
    : VirtualDom_Attribute<'event> =
    VirtualDom_ModifierProperty
        { Key = StringRope.toString key
          Value = value }

let inline VirtualDom_attribute
    (key: StringRope)
    (value: StringRope)
    : VirtualDom_Attribute<'event> =
    VirtualDom_ModifierAttribute
        { Namespace = ValueNone
          Key = StringRope.toString key
          Value = StringRope.toString value }

let inline VirtualDom_attributeNS
    (namespace_: StringRope)
    (key: StringRope)
    (value: StringRope)
    : VirtualDom_Attribute<'event> =
    VirtualDom_ModifierAttribute
        { Namespace = ValueSome(StringRope.toString namespace_)
          Key = StringRope.toString key
          Value = StringRope.toString value }

let inline VirtualDom_on
    (name: StringRope)
    (handler: VirtualDom_Handler<'event>)
    : VirtualDom_Attribute<'event> =
    VirtualDom_ModifierEventListener
        { Name = StringRope.toString name
          Handler = handler }

let VirtualDom_mapAttribute
    (eventChange: 'event -> 'eventMapped)
    (modifier: VirtualDom_Attribute<'event>)
    : VirtualDom_Attribute<'eventMapped> =
    match modifier with
    | VirtualDom_ModifierAttribute virtualDomAttribute ->
        VirtualDom_ModifierAttribute virtualDomAttribute
    | VirtualDom_ModifierStyle virtualDomStyle ->
        VirtualDom_ModifierStyle virtualDomStyle
    | VirtualDom_ModifierProperty virtualDomProperty ->
        VirtualDom_ModifierProperty virtualDomProperty
    | VirtualDom_ModifierEventListener virtualDomEventListener ->
        VirtualDom_ModifierEventListener
            { Name = virtualDomEventListener.Name
              Handler =
                VirtualDom_handlerMap eventChange virtualDomEventListener.Handler }

let rec VirtualDom_elementMap
    (eventChange: 'event -> 'eventMapped)
    (element: VirtualDom_Element<'event>)
    : VirtualDom_Element<'eventMapped> =
    { Tag = element.Tag
      Namespace = element.Namespace
      Subs = List.map (fun sub -> VirtualDom_map eventChange sub) element.Subs
      Modifiers =
        List.map
            (fun modifier -> VirtualDom_mapAttribute eventChange modifier)
            element.Modifiers }

and VirtualDom_elementKeyedMap
    (eventChange: 'event -> 'eventMapped)
    (element: VirtualDom_ElementKeyed<'event>)
    : VirtualDom_ElementKeyed<'eventMapped> =
    { Tag = element.Tag
      Namespace = element.Namespace
      Subs =
        List.map
            (fun subKeyed ->
                { Key = subKeyed.Key
                  Node = VirtualDom_map eventChange subKeyed.Node })
            element.Subs
      Modifiers =
        List.map
            (fun modifier -> VirtualDom_mapAttribute eventChange modifier)
            element.Modifiers }

and VirtualDom_NodeLazyMap
    (eventChange: 'event -> 'eventMapped)
    (nodeLazy: VirtualDom_NodeLazy<'event>)
    : VirtualDom_NodeLazy<'eventMapped> =
    { Keys = nodeLazy.Keys
      Construct = fun () -> VirtualDom_map eventChange (nodeLazy.Construct()) }

and VirtualDom_map
    (eventChange: 'event -> 'eventMapped)
    (node: VirtualDom_Node<'event>)
    : VirtualDom_Node<'eventMapped> =
    match node with
    | VirtualDom_Text text -> VirtualDom_Text text
    | VirtualDom_Element element ->
        VirtualDom_Element(VirtualDom_elementMap eventChange element)
    | VirtualDom_ElementKeyed element ->
        VirtualDom_ElementKeyed(VirtualDom_elementKeyedMap eventChange element)
    | VirtualDom_NodeLazy nodeLazy ->
        VirtualDom_NodeLazy(VirtualDom_NodeLazyMap eventChange nodeLazy)

let inline VirtualDom_lazy
    (construct: 'a -> VirtualDom_Node<'event>)
    (a: 'a)
    : VirtualDom_Node<'event> =
    VirtualDom_NodeLazy
        { Keys = [| a |]
          Construct = fun () -> construct a }

let inline VirtualDom_lazy2
    (construct: 'a -> 'b -> VirtualDom_Node<'event>)
    (a: 'a)
    (b: 'b)
    : VirtualDom_Node<'event> =
    VirtualDom_NodeLazy
        { Keys = [| a; b |]
          Construct = fun () -> construct a b }

let inline VirtualDom_lazy3
    (construct: 'a -> 'b -> 'c -> VirtualDom_Node<'event>)
    (a: 'a)
    (b: 'b)
    (c: 'c)
    : VirtualDom_Node<'event> =
    VirtualDom_NodeLazy
        { Keys = [| a; b; c |]
          Construct = fun () -> construct a b c }

let inline VirtualDom_lazy4
    (construct: 'a -> 'b -> 'c -> 'd -> VirtualDom_Node<'event>)
    (a: 'a)
    (b: 'b)
    (c: 'c)
    (d: 'd)
    : VirtualDom_Node<'event> =
    VirtualDom_NodeLazy
        { Keys = [| a; b; c; d |]
          Construct = fun () -> construct a b c d }

let inline VirtualDom_lazy5
    (construct: 'a -> 'b -> 'c -> 'd -> 'e -> VirtualDom_Node<'event>)
    (a: 'a)
    (b: 'b)
    (c: 'c)
    (d: 'd)
    (e: 'e)
    : VirtualDom_Node<'event> =
    VirtualDom_NodeLazy
        { Keys = [| a; b; c; d; e |]
          Construct = fun () -> construct a b c d e }

let inline VirtualDom_lazy6
    (construct: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> VirtualDom_Node<'event>)
    (a: 'a)
    (b: 'b)
    (c: 'c)
    (d: 'd)
    (e: 'e)
    (f: 'f)
    : VirtualDom_Node<'event> =
    VirtualDom_NodeLazy
        { Keys = [| a; b; c; d; e; f |]
          Construct = fun () -> construct a b c d e f }

let inline VirtualDom_lazy7
    (construct: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> VirtualDom_Node<'event>)
    (a: 'a)
    (b: 'b)
    (c: 'c)
    (d: 'd)
    (e: 'e)
    (f: 'f)
    (g: 'g)
    : VirtualDom_Node<'event> =
    VirtualDom_NodeLazy
        { Keys = [| a; b; c; d; e; f; g |]
          Construct = fun () -> construct a b c d e f g }

let inline VirtualDom_lazy8
    (construct:
        'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> VirtualDom_Node<'event>)
    (a: 'a)
    (b: 'b)
    (c: 'c)
    (d: 'd)
    (e: 'e)
    (f: 'f)
    (g: 'g)
    (h: 'h)
    : VirtualDom_Node<'event> =
    VirtualDom_NodeLazy
        { Keys = [| a; b; c; d; e; f; g; h |]
          Construct = fun () -> construct a b c d e f g h }


let inline MathVector2_vec2 (x: float) (y: float) : System.Numerics.Vector2 =
    System.Numerics.Vector2(float32 x, float32 y)

let inline MathVector2_getX (vector2: System.Numerics.Vector2) : float =
    float vector2.X

let inline MathVector2_getY (vector2: System.Numerics.Vector2) : float =
    float vector2.Y

let inline MathVector2_setX
    (newX: float)
    (vector2: System.Numerics.Vector2)
    : System.Numerics.Vector2 =
    System.Numerics.Vector2(float32 newX, vector2.Y)

let inline MathVector2_setY
    (newY: float)
    (vector2: System.Numerics.Vector2)
    : System.Numerics.Vector2 =
    System.Numerics.Vector2(vector2.X, float32 newY)

let inline MathVector2_add
    (a: System.Numerics.Vector2)
    (b: System.Numerics.Vector2)
    : System.Numerics.Vector2 =
    System.Numerics.Vector2.Add(a, b)

let inline MathVector2_sub
    (baseVector2: System.Numerics.Vector2)
    (toSubtract: System.Numerics.Vector2)
    : System.Numerics.Vector2 =
    System.Numerics.Vector2.Subtract(baseVector2, toSubtract)

let inline MathVector2_scale
    (factor: float)
    (vector2: System.Numerics.Vector2)
    : System.Numerics.Vector2 =
    System.Numerics.Vector2.Multiply(vector2, float32 factor)

let inline MathVector2_dot
    (a: System.Numerics.Vector2)
    (b: System.Numerics.Vector2)
    : float =
    float (System.Numerics.Vector2.Dot(a, b))

let inline MathVector2_direction
    (a: System.Numerics.Vector2)
    (b: System.Numerics.Vector2)
    : System.Numerics.Vector2 =
    System.Numerics.Vector2.Normalize(System.Numerics.Vector2.Subtract(a, b))

let inline MathVector2_length (vector2: System.Numerics.Vector2) : float =
    float (vector2.Length())

let inline MathVector2_lengthSquared (vector2: System.Numerics.Vector2) : float =
    float (vector2.LengthSquared())

let inline MathVector2_distance
    (a: System.Numerics.Vector2)
    (b: System.Numerics.Vector2)
    : float =
    float (System.Numerics.Vector2.Distance(a, b))

let inline MathVector2_distanceSquared
    (a: System.Numerics.Vector2)
    (b: System.Numerics.Vector2)
    : float =
    float (System.Numerics.Vector2.DistanceSquared(a, b))

[<Struct>]
type MathVector2_AsRecord = { X: float; Y: float }

let inline MathVector2_toRecord
    (vector2: System.Numerics.Vector2)
    : MathVector2_AsRecord =
    { X = float vector2.X
      Y = float vector2.Y }

let inline MathVector2_fromRecord
    (asRecord: MathVector2_AsRecord)
    : System.Numerics.Vector2 =
    System.Numerics.Vector2(float32 asRecord.X, float32 asRecord.Y)



let MathVector3_vec3 (x: float) (y: float) (z: float) : System.Numerics.Vector3 =
    System.Numerics.Vector3(float32 x, float32 y, float32 z)

let MathVector3_i: System.Numerics.Vector3 = System.Numerics.Vector3(1f, 0f, 0f)

let MathVector3_j: System.Numerics.Vector3 = System.Numerics.Vector3(0f, 1f, 0f)

let MathVector3_k: System.Numerics.Vector3 = System.Numerics.Vector3(0f, 0f, 1f)

let MathVector3_getX (vector3: System.Numerics.Vector3) : float = float vector3.X

let MathVector3_getY (vector3: System.Numerics.Vector3) : float = float vector3.Y

let MathVector3_getZ (vector3: System.Numerics.Vector3) : float = float vector3.Z

let MathVector3_setX
    (newX: float)
    (vector3: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3(float32 newX, vector3.Y, vector3.Z)

let MathVector3_setY
    (newY: float)
    (vector3: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3(vector3.X, float32 newY, vector3.Z)

let MathVector3_setZ
    (newZ: float)
    (vector3: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3(vector3.X, vector3.Y, float32 newZ)

let MathVector3_add
    (a: System.Numerics.Vector3)
    (b: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3.Add(a, b)

let MathVector3_sub
    (baseVector3: System.Numerics.Vector3)
    (toSubtract: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3.Subtract(baseVector3, toSubtract)

let MathVector3_scale
    (factor: float)
    (vector3: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3.Multiply(vector3, float32 factor)

let MathVector3_dot
    (a: System.Numerics.Vector3)
    (b: System.Numerics.Vector3)
    : float =
    float (System.Numerics.Vector3.Dot(a, b))

let MathVector3_cross
    (a: System.Numerics.Vector3)
    (b: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3.Cross(a, b)

let MathVector3_direction
    (a: System.Numerics.Vector3)
    (b: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3.Normalize(System.Numerics.Vector3.Subtract(a, b))

let MathVector3_length (vector3: System.Numerics.Vector3) : float =
    float (vector3.Length())

let MathVector3_lengthSquared (vector3: System.Numerics.Vector3) : float =
    float (vector3.LengthSquared())

let MathVector3_distance
    (a: System.Numerics.Vector3)
    (b: System.Numerics.Vector3)
    : float =
    float (System.Numerics.Vector3.Distance(a, b))

let MathVector3_distanceSquared
    (a: System.Numerics.Vector3)
    (b: System.Numerics.Vector3)
    : float =
    float (System.Numerics.Vector3.DistanceSquared(a, b))

[<Struct>]
type MathVector3_AsRecord = { X: float; Y: float; Z: float }

let inline MathVector3_toRecord
    (vector3: System.Numerics.Vector3)
    : MathVector3_AsRecord =
    { X = float vector3.X
      Y = float vector3.Y
      Z = float vector3.Z }

let inline MathVector3_fromRecord
    (asRecord: MathVector3_AsRecord)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3(
        float32 asRecord.X,
        float32 asRecord.Y,
        float32 asRecord.Z
    )



let inline MathVector4_vec4
    (x: float)
    (y: float)
    (z: float)
    (w: float)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4(float32 x, float32 y, float32 z, float32 w)

let inline MathVector4_getX (vector4: System.Numerics.Vector4) : float =
    float vector4.X

let inline MathVector4_getY (vector4: System.Numerics.Vector4) : float =
    float vector4.Y

let inline MathVector4_getZ (vector4: System.Numerics.Vector4) : float =
    float vector4.Z

let inline MathVector4_getW (vector4: System.Numerics.Vector4) : float =
    float vector4.W

let inline MathVector4_setX
    (newX: float)
    (vector4: System.Numerics.Vector4)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4(float32 newX, vector4.Y, vector4.Z, vector4.W)

let inline MathVector4_setY
    (newY: float)
    (vector4: System.Numerics.Vector4)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4(vector4.X, float32 newY, vector4.Z, vector4.W)

let inline MathVector4_setZ
    (newZ: float)
    (vector4: System.Numerics.Vector4)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4(vector4.X, vector4.Y, float32 newZ, vector4.W)

let inline MathVector4_setW
    (newW: float)
    (vector4: System.Numerics.Vector4)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4(vector4.X, vector4.Y, vector4.Z, float32 newW)

let inline MathVector4_add
    (a: System.Numerics.Vector4)
    (b: System.Numerics.Vector4)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4.Add(a, b)

let inline MathVector4_sub
    (baseVector4: System.Numerics.Vector4)
    (toSubtract: System.Numerics.Vector4)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4.Subtract(baseVector4, toSubtract)

let inline MathVector4_scale
    (factor: float)
    (vector4: System.Numerics.Vector4)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4.Multiply(vector4, float32 factor)

let inline MathVector4_dot
    (a: System.Numerics.Vector4)
    (b: System.Numerics.Vector4)
    : float =
    float (System.Numerics.Vector4.Dot(a, b))

let inline MathVector4_direction
    (a: System.Numerics.Vector4)
    (b: System.Numerics.Vector4)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4.Normalize(System.Numerics.Vector4.Subtract(a, b))

let inline MathVector4_length (vector4: System.Numerics.Vector4) : float =
    float (vector4.Length())

let inline MathVector4_lengthSquared (vector4: System.Numerics.Vector4) : float =
    float (vector4.LengthSquared())

let inline MathVector4_distance
    (a: System.Numerics.Vector4)
    (b: System.Numerics.Vector4)
    : float =
    float (System.Numerics.Vector4.Distance(a, b))

let inline MathVector4_distanceSquared
    (a: System.Numerics.Vector4)
    (b: System.Numerics.Vector4)
    : float =
    float (System.Numerics.Vector4.DistanceSquared(a, b))

[<Struct>]
type MathVector4_AsRecord =
    { X: float
      Y: float
      Z: float
      W: float }

let inline MathVector4_toRecord
    (vector4: System.Numerics.Vector4)
    : MathVector4_AsRecord =
    { X = float vector4.X
      Y = float vector4.Y
      Z = float vector4.Z
      W = float vector4.W }

let inline MathVector4_fromRecord
    (asRecord: MathVector4_AsRecord)
    : System.Numerics.Vector4 =
    System.Numerics.Vector4(
        float32 asRecord.X,
        float32 asRecord.Y,
        float32 asRecord.Z,
        float32 asRecord.W
    )



let MathMatrix4_inverse
    (matrix4: System.Numerics.Matrix4x4)
    : ValueOption<System.Numerics.Matrix4x4> =
    let (wasSuccessful, result) = System.Numerics.Matrix4x4.Invert matrix4
    if wasSuccessful then ValueSome result else ValueNone

let inline MathMatrix4_mul
    (a: System.Numerics.Matrix4x4)
    (b: System.Numerics.Matrix4x4)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.Multiply(a, b)

let inline MathMatrix4_makeBasis
    (vx: System.Numerics.Vector3)
    (vy: System.Numerics.Vector3)
    (vz: System.Numerics.Vector3)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4(
        vx.X,
        vx.Y,
        vx.Z,
        0f,
        vy.X,
        vy.Y,
        vy.Z,
        0f,
        vz.X,
        vz.Y,
        vz.Z,
        0f,
        0f,
        0f,
        0f,
        1f
    )

let inline MathMatrix4_transform
    (matrix4: System.Numerics.Matrix4x4)
    (position: System.Numerics.Vector3)
    : System.Numerics.Vector3 =
    System.Numerics.Vector3.Transform(position, matrix4)

let MathMatrix4_makeFrustum
    (left: float)
    (right: float)
    (bottom: float)
    (top: float)
    (zNearPlane: float)
    (zFarPlane: float)
    : System.Numerics.Matrix4x4 =
    let left32: float32 = float32 left
    let right32: float32 = float32 right
    let bottom32: float32 = float32 bottom
    let top32: float32 = float32 top
    let zNearPlane32: float32 = float32 zNearPlane
    let zFarPlane32: float32 = float32 zFarPlane

    System.Numerics.Matrix4x4(
        2f * zNearPlane32 / (right32 - left32),
        0f,
        0f,
        0f,
        0f,
        2f * zNearPlane32 / (top32 - bottom32),
        0f,
        0f,
        (right32 + left32) / (right32 - left32),
        (top32 + bottom32) / (top32 - bottom32),
        -(zFarPlane32 + zNearPlane32) / (zFarPlane32 - zNearPlane32),
        -1f,
        0f,
        0f,
        -2f * zFarPlane32 * zNearPlane32 / (zFarPlane32 - zNearPlane32),
        0f
    )

let inline MathMatrix4_makePerspective
    (fieldOfView: float)
    (aspectRatio: float)
    (nearPlaneDistance: float)
    (farPlaneDistance: float)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.CreatePerspectiveFieldOfView(
        float32 fieldOfView,
        float32 aspectRatio,
        float32 nearPlaneDistance,
        float32 farPlaneDistance
    )

let inline MathMatrix4_makeOrtho
    (left: float)
    (right: float)
    (bottom: float)
    (top: float)
    (zNearPlane: float)
    (zFarPlane: float)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.CreateOrthographicOffCenter(
        float32 left,
        float32 right,
        float32 bottom,
        float32 top,
        float32 zNearPlane,
        float32 zFarPlane
    )

let inline MathMatrix4_makeOrtho2D
    (left: float)
    (right: float)
    (bottom: float)
    (top: float)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.CreateOrthographicOffCenter(
        float32 left,
        float32 right,
        float32 bottom,
        float32 top,
        -1f,
        1f
    )

let inline MathMatrix4_makeLookAt
    (cameraPosition: System.Numerics.Vector3)
    (cameraTarget: System.Numerics.Vector3)
    (cameraUpVector: System.Numerics.Vector3)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.CreateLookAt(
        cameraPosition,
        cameraTarget,
        cameraUpVector
    )

let inline MathMatrix4_rotate
    (angle: float)
    (axis: System.Numerics.Vector3)
    (matrix4: System.Numerics.Matrix4x4)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.Multiply(
        matrix4,
        System.Numerics.Matrix4x4.CreateFromAxisAngle(axis, float32 angle)
    )

let inline MathMatrix4_scale
    (scales: System.Numerics.Vector3)
    (matrix4: System.Numerics.Matrix4x4)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.Multiply(
        matrix4,
        System.Numerics.Matrix4x4.CreateScale scales
    )

let inline MathMatrix4_scale3
    (xScale: float)
    (yScale: float)
    (zScale: float)
    (matrix4: System.Numerics.Matrix4x4)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.Multiply(
        matrix4,
        System.Numerics.Matrix4x4.CreateScale(
            float32 xScale,
            float32 yScale,
            float32 zScale
        )
    )

let inline MathMatrix4_translate
    (position: System.Numerics.Vector3)
    (matrix4: System.Numerics.Matrix4x4)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.Multiply(
        matrix4,
        System.Numerics.Matrix4x4.CreateTranslation position
    )

let inline MathMatrix4_translate3
    (xPosition: float)
    (yPosition: float)
    (zPosition: float)
    (matrix4: System.Numerics.Matrix4x4)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.Multiply(
        matrix4,
        System.Numerics.Matrix4x4.CreateTranslation(
            float32 xPosition,
            float32 yPosition,
            float32 zPosition
        )
    )

let inline MathMatrix4_makeRotate
    (angle: float)
    (axis: System.Numerics.Vector3)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.CreateFromAxisAngle(axis, float32 angle)

let inline MathMatrix4_makeScale3
    (xScale: float)
    (yScale: float)
    (zScale: float)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.CreateScale(
        float32 xScale,
        float32 yScale,
        float32 zScale
    )

let inline MathMatrix4_makeTranslate3
    (xPosition: float)
    (yPosition: float)
    (zPosition: float)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4.CreateTranslation(
        float32 xPosition,
        float32 yPosition,
        float32 zPosition
    )

[<Struct>]
type MathMatrix4_AsRecord =
    { M11: float
      M21: float
      M31: float
      M41: float
      M12: float
      M22: float
      M32: float
      M42: float
      M13: float
      M23: float
      M33: float
      M43: float
      M14: float
      M24: float
      M34: float
      M44: float }

let inline MathMatrix4_toRecord
    (matrix4: System.Numerics.Matrix4x4)
    : MathMatrix4_AsRecord =
    { M11 = float matrix4.M11
      M21 = float matrix4.M21
      M31 = float matrix4.M31
      M41 = float matrix4.M41
      M12 = float matrix4.M12
      M22 = float matrix4.M22
      M32 = float matrix4.M32
      M42 = float matrix4.M42
      M13 = float matrix4.M13
      M23 = float matrix4.M23
      M33 = float matrix4.M33
      M43 = float matrix4.M43
      M14 = float matrix4.M14
      M24 = float matrix4.M24
      M34 = float matrix4.M34
      M44 = float matrix4.M44 }

let inline MathMatrix4_fromRecord
    (asRecord: MathMatrix4_AsRecord)
    : System.Numerics.Matrix4x4 =
    System.Numerics.Matrix4x4(
        float32 asRecord.M11,
        float32 asRecord.M12,
        float32 asRecord.M13,
        float32 asRecord.M14,
        float32 asRecord.M21,
        float32 asRecord.M22,
        float32 asRecord.M23,
        float32 asRecord.M24,
        float32 asRecord.M31,
        float32 asRecord.M32,
        float32 asRecord.M33,
        float32 asRecord.M34,
        float32 asRecord.M41,
        float32 asRecord.M42,
        float32 asRecord.M43,
        float32 asRecord.M44
    )


[<Struct>]
type PlatformCmd_PortOutgoing =
    { Name: string
      Value: System.Text.Json.Nodes.JsonNode }

[<Struct>]
type PlatformCmd_CmdSingle<'event> =
    | PlatformCmd_PortOutgoing of PlatformCmd_PortOutgoing: PlatformCmd_PortOutgoing

type PlatformCmd_Cmd<'event> = List<PlatformCmd_CmdSingle<'event>>

let PlatformCmd_none: PlatformCmd_Cmd<'event> = []

let inline PlatformCmd_batch
    (subCommands: List<PlatformCmd_Cmd<'event>>)
    : PlatformCmd_Cmd<'event> =
    List.concat subCommands

let PlatformCmd_singleMap
    (_eventChange: 'event -> 'mappedEvent)
    (commandSingle: PlatformCmd_CmdSingle<'event>)
    : PlatformCmd_CmdSingle<'mappedEvent> =
    match commandSingle with
    | PlatformCmd_PortOutgoing portOutgoing -> PlatformCmd_PortOutgoing portOutgoing

let inline PlatformCmd_map
    ([<InlineIfLambda>] eventChange: 'event -> 'mappedEvent)
    (command: PlatformCmd_Cmd<'event>)
    : PlatformCmd_Cmd<'mappedEvent> =
    List.map (fun single -> PlatformCmd_singleMap eventChange single) command

let inline PlatformCmd_portOutgoingWithName
    (name: StringRope)
    (value: System.Text.Json.Nodes.JsonNode)
    : PlatformCmd_Cmd<'event> =
    [ PlatformCmd_PortOutgoing
          { Name = StringRope.toString name
            Value = value } ]

[<Struct>]
type PlatformSub_PortIncoming<'event> =
    { Name: string
      OnValue: System.Text.Json.Nodes.JsonNode -> 'event }

[<Struct>]
type PlatformSub_SubSingle<'event> =
    | PlatformSub_PortIncoming of
        PlatformSub_PortIncoming: PlatformSub_PortIncoming<'event>

type PlatformSub_Sub<'event> = List<PlatformSub_SubSingle<'event>>

let PlatformSub_none: PlatformSub_Sub<'event> = []

let inline PlatformSub_batch
    (subSubscriptions: List<PlatformSub_Sub<'event>>)
    : PlatformSub_Sub<'event> =
    List.concat subSubscriptions

let PlatformSub_singleMap
    (eventChange: 'event -> 'mappedEvent)
    (subscriptionSingle: PlatformSub_SubSingle<'event>)
    : PlatformSub_SubSingle<'mappedEvent> =
    match subscriptionSingle with
    | PlatformSub_PortIncoming portIncoming ->
        PlatformSub_PortIncoming
            { Name = portIncoming.Name
              OnValue = fun value -> eventChange (portIncoming.OnValue value) }

let inline PlatformSub_map
    (eventChange: 'event -> 'mappedEvent)
    (subscription: PlatformSub_Sub<'event>)
    : PlatformSub_Sub<'mappedEvent> =
    List.map (fun single -> PlatformSub_singleMap eventChange single) subscription

let inline PlatformSub_portIncomingWithName
    (name: StringRope)
    (onValue: System.Text.Json.Nodes.JsonNode -> 'event)
    : PlatformSub_Sub<'event> =
    [ PlatformSub_PortIncoming
          { Name = StringRope.toString name
            OnValue = onValue } ]

[<Struct>]
type Platform_Program<'flags, 'state, 'event> =
    { Init: 'flags -> struct ('state * PlatformCmd_Cmd<'event>)
      Update: 'event -> 'state -> struct ('state * PlatformCmd_Cmd<'event>)
      Subscriptions: 'state -> PlatformSub_Sub<'event> }

let inline Platform_worker
    (config: Platform_Program<'flags, 'state, 'event>)
    : Platform_Program<'flags, 'state, 'event> =
    config
