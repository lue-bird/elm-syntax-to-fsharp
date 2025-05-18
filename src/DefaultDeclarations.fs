namespace global

module Elm =
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

    let inline Basics_ceiling (n: float) : int64 = int64 (System.Math.Ceiling(n))
    let inline Basics_floor (n: float) : int64 = int64 (System.Math.Floor(n))
    let inline Basics_round (n: float) : int64 = int64 (System.Math.Round(n))
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

        if (remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0) then
            remainder + toDivide

        else
            remainder

    let inline Basics_fpow (a: float) (b: float) : float = a ** b

    let inline Basics_ipow (a: int64) (b: int64) : int64 =
        int64 (float a ** float b)

    let inline Basics_iclamp (minimum: int64) (maximum: int64) (n: int64) : int64 =
        System.Math.Clamp(value = n, min = minimum, max = maximum)

    let inline Basics_fclamp (minimum: float) (maximum: float) (n: float) : float =
        System.Math.Clamp(value = n, min = minimum, max = maximum)

    let inline Basics_logBase (newBase: float) (n: float) : float =
        System.Math.Log(n, newBase = newBase)

    let inline Basics_atan2 (y: float) (x: float) : float =
        System.Double.Atan2(y, x)

    let inline Basics_radians (radians: float) : float = radians

    let inline Basics_degrees (angleInDegrees: float) : float =
        (angleInDegrees * System.Math.PI) / 180.0

    let inline Basics_turns (angleInTurns: float) : float =
        (System.Math.PI * 2.0) * angleInTurns

    let Basics_fromPolar
        (struct (radius: float, theta: float))
        : struct (float * float) =
        struct (radius * (System.Double.Cos(theta)),
                radius * (System.Double.Sin(theta)))

    let Basics_toPolar (struct (x: float, y: float)) : struct (float * float) =
        struct (System.Double.Sqrt((x * x) + (y * y)), System.Double.Atan2(y, x))

    let inline Bitwise_shiftLeftBy
        (bitPositionsToShiftBy: int64)
        (n: int64)
        : int64 =
        n <<< int32 bitPositionsToShiftBy

    let inline Bitwise_shiftRightBy
        (bitPositionsToShiftBy: int64)
        (n: int64)
        : int64 =
        n >>> int32 bitPositionsToShiftBy

    let inline Bitwise_shiftRightZfBy
        (bitPositionsToShiftBy: int64)
        (n: int64)
        : int64 =
        int64 (int64 n >>> int32 bitPositionsToShiftBy)

    let inline Basics_and (a: bool) (b: bool) : bool = a && b
    let inline Basics_or (a: bool) (b: bool) : bool = a || b

    type Result_Result<'error, 'value> = Result<'value, 'error>

    type Basics_Never = JustOneMore of Basics_Never
    let rec Basics_never (JustOneMore ever: Basics_Never) = Basics_never ever

    let inline Char_isOctDigit (ch: char) : bool =
        let code = int ch

        code <= 0x37 && 0x30 <= code

    [<CustomEquality; CustomComparison>]
    type StringRope =
        | StringRopeOne of string
        | StringRopeAppend of StringRope * StringRope

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
                    | StringRopeAppend(left, right) ->
                        stringRopeToMatchNext <- left
                        mutableRemainingRightStringRopes.Push(right)

                mutableBuilder.ToString()

        override x.GetHashCode() = hash (StringRope.toString x)

        override x.Equals(other) =
            match other with
            | :? StringRope as otherStringRope ->
                StringRope.toString x = StringRope.toString otherStringRope
            | _ -> false

        interface System.IComparable with
            member x.CompareTo(other) =
                match other with
                | :? StringRope as otherStringRope ->
                    (StringRope.toString x)
                        .CompareTo(StringRope.toString otherStringRope)
                | _ -> -1

    let stringRopeEmpty: StringRope = StringRopeOne ""

    let rec String_isEmpty (stringToCheck: StringRope) : bool =
        match stringToCheck with
        | StringRopeOne string -> System.String.IsNullOrEmpty(string)
        | StringRopeAppend(left, right) ->
            String_isEmpty left && String_isEmpty right

    let inline String_length (str: StringRope) : int64 =
        String.length (StringRope.toString str)

    let inline String_repeat
        (repetitions: int64)
        (segment: StringRope)
        : StringRope =
        StringRopeOne(
            String.replicate (int repetitions) (StringRope.toString segment)
        )

    let String_toList (string: StringRope) : List<char> =
        List.ofArray ((StringRope.toString string).ToCharArray())

    let inline String_fromList (chars: List<char>) : StringRope =
        StringRopeOne(new string (List.toArray chars))

    let inline String_contains
        (substringRope: StringRope)
        (string: StringRope)
        : bool =
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
        StringRopeOne(String.map charChange (StringRope.toString string))

    let inline String_filter
        ([<InlineIfLambda>] charShouldBeKept: char -> bool)
        (string: StringRope)
        : StringRope =
        StringRopeOne(String.filter charShouldBeKept (StringRope.toString string))

    let inline String_foldl
        ([<InlineIfLambda>] reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: StringRope)
        : 'folded =
        Seq.fold
            (fun soFar char -> reduce char soFar)
            initialFolded
            (StringRope.toString string)

    let inline String_foldr
        ([<InlineIfLambda>] reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: StringRope)
        : 'folded =
        Seq.foldBack reduce (StringRope.toString string) initialFolded

    let inline String_trim (string: StringRope) : StringRope =
        StringRopeOne((StringRope.toString string).Trim())

    let inline String_trimLeft (string: StringRope) : StringRope =
        StringRopeOne((StringRope.toString string).TrimStart())

    let inline String_trimRight (string: StringRope) : StringRope =
        StringRopeOne((StringRope.toString string).TrimEnd())

    let String_right
        (takenElementCount: int64)
        (stringRope: StringRope)
        : StringRope =
        let string: string = StringRope.toString stringRope

        StringRopeOne(
            string.Substring(
                String.length string - int takenElementCount,
                int takenElementCount
            )
        )

    let inline String_left
        (skippedElementCount: int64)
        (string: StringRope)
        : StringRope =
        StringRopeOne(
            (StringRope.toString string).Substring(0, int skippedElementCount)
        )

    let String_dropRight
        (skippedElementCount: int64)
        (stringRope: StringRope)
        : StringRope =
        let string: string = StringRope.toString stringRope

        StringRopeOne(
            string.Substring(0, String.length string - int skippedElementCount)
        )

    let String_dropLeft
        (skippedElementCount: int64)
        (stringRope: StringRope)
        : StringRope =
        let string: string = StringRope.toString stringRope

        StringRopeOne(
            string.Substring(
                int skippedElementCount,
                String.length string - int skippedElementCount
            )
        )

    let inline String_append (early: StringRope) (late: StringRope) : StringRope =
        StringRopeAppend(early, late)

    let inline String_fromChar (char: char) : StringRope =
        StringRopeOne(string char)

    let inline String_cons (newHeadChar: char) (late: StringRope) : StringRope =
        StringRopeAppend(StringRopeOne(string newHeadChar), late)

    let String_uncons
        (stringRope: StringRope)
        : option<struct (char * StringRope)> =
        let string: string = StringRope.toString stringRope

        if System.String.IsNullOrEmpty(string) then
            None
        else
            Some(struct (string[0], StringRopeOne(string[1..])))

    let String_split
        (separator: StringRope)
        (string: StringRope)
        : List<StringRope> =
        // can be optimized
        List.ofArray (
            Array.map
                (fun segment -> StringRopeOne segment)
                ((StringRope.toString string).Split(StringRope.toString separator))
        )

    let String_lines (string: StringRope) : List<StringRope> =
        // can be optimized
        List.ofArray (
            (Array.map
                (fun line -> StringRopeOne line)
                ((StringRope.toString string).Replace("\r\n", "\n").Split("\n")))
        )

    let String_reverse (string: StringRope) : StringRope =
        StringRopeOne(
            new string (Array.rev ((StringRope.toString string).ToCharArray()))
        )

    let inline String_replace
        (toReplace: StringRope)
        (replacement: StringRope)
        (string: StringRope)
        : StringRope =
        StringRopeOne(
            (StringRope.toString string)
                .Replace(
                    StringRope.toString toReplace,
                    StringRope.toString replacement
                )
        )

    let inline String_toUpper (string: StringRope) : StringRope =
        StringRopeOne((StringRope.toString string).ToUpperInvariant())

    let inline String_toLower (string: StringRope) : StringRope =
        StringRopeOne((StringRope.toString string).ToLowerInvariant())

    let String_join
        (separator: StringRope)
        (strings: List<StringRope>)
        : StringRope =
        // can be optimized
        StringRopeOne(
            String.concat
                (StringRope.toString separator)
                (List.map StringRope.toString strings)
        )

    let String_concat (strings: List<StringRope>) : StringRope =
        // can be optimized
        StringRopeOne(System.String.Concat(List.map StringRope.toString strings))

    let inline String_padLeft
        (newMinimumLength: int64)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne(
            (StringRope.toString string).PadLeft(int newMinimumLength, padding)
        )

    let inline String_padRight
        (newMinimumLength: int64)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne(
            (StringRope.toString string).PadRight(int newMinimumLength, padding)
        )

    let inline String_fromFloat (n: float) : StringRope = StringRopeOne(string n)
    let inline String_fromInt (n: int64) : StringRope = StringRopeOne(string n)

    let String_toInt (string: StringRope) : option<int64> =
        let (success, num) = System.Int64.TryParse(StringRope.toString string)

        if success then Some num else None

    let String_toFloat (string: StringRope) : option<float> =
        let (success, num) = System.Double.TryParse(StringRope.toString string)

        if success then Some num else None

    let String_slice
        (startInclusivePossiblyNegative: int64)
        (endExclusivePossiblyNegative: int64)
        (stringRope: StringRope)
        : StringRope =
        let string = StringRope.toString stringRope

        let realStartIndex: int =
            if (startInclusivePossiblyNegative < 0L) then
                max 0 (int startInclusivePossiblyNegative + String.length string)
            else
                int startInclusivePossiblyNegative

        let realEndIndexExclusive: int =
            if (endExclusivePossiblyNegative < 0L) then
                max 0 (int endExclusivePossiblyNegative + String.length string)
            else
                min (int endExclusivePossiblyNegative) (String.length string)

        if (realStartIndex >= realEndIndexExclusive) then
            stringRopeEmpty
        else
            StringRopeOne(
                string.Substring(
                    realStartIndex,
                    realEndIndexExclusive - realStartIndex
                )
            )

    let inline List_length (list: List<'a>) : int64 = List.length list

    let inline List_tail (list: List<'a>) : option<List<'a>> =
        match list with
        | [] -> None
        | head :: tail -> Some tail

    let inline List_member (needle: 'a) (list: List<'a>) : bool =
        List.contains needle list

    let List_minimum (list: List<'a>) : option<'a> =
        match list with
        | [] -> None
        | _ :: _ -> Some(List.min list)

    let List_maximum (list: List<'a>) : option<'a> =
        match list with
        | [] -> None
        | _ :: _ -> Some(List.max list)

    let List_fproduct (list: List<float>) : float = List.fold (*) 1.0 list
    let List_iproduct (list: List<int64>) : int64 = List.fold (*) 1L list

    let inline List_cons (newHead: 'a) (tail: List<'a>) : List<'a> = newHead :: tail

    let inline List_repeat (repetitions: int64) (element: 'a) : List<'a> =
        List.replicate (int repetitions) element

    let inline List_take
        (elementCountFromStart: int64)
        (list: List<'a>)
        : List<'a> =
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

    let List_intersperse (sep: 'a) (list: List<'a>) =
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

    let inline Dict_size (dict: Map<'key, 'value>) : int64 = Map.count dict

    let inline Dict_singleton (key: 'key) (value: 'value) : Map<'key, 'value> =
        Map [ (key, value) ]

    let inline Dict_toList
        (dict: Map<'key, 'value>)
        : List<struct ('key * 'value)> =
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
            =
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

    let Array_get (index: int64) (array: array<'element>) : option<'element> =
        Array.tryItem (int index) array

    let inline Array_initialize
        (count: int64)
        ([<InlineIfLambda>] indexToElement: int64 -> 'element)
        : array<'element> =
        Array.init (max 0 (int count)) (fun index -> indexToElement index)

    let inline Array_repeat (count: int64) (element: 'element) : array<'element> =
        Array.replicate (max 0 (int count)) element

    let Array_set
        (index: int64)
        (replacementElement: 'element)
        (array: array<'element>)
        : array<'element> =
        if index < 0 then array
        else if index >= Array.length array then array
        else Array.updateAt (int index) replacementElement array

    let Array_push
        (newLastElement: 'element)
        (array: array<'element>)
        : array<'element> =
        Array.append array [| newLastElement |]

    let inline Array_indexedMap
        ([<InlineIfLambda>] elementChange: int64 -> 'a -> 'b)
        (array: array<'a>)
        : array<'b> =
        Array.mapi (fun index element -> elementChange index element) array

    let Array_toIndexedList (array: array<'a>) : List<struct (int64 * 'a)> =
        (Array.foldBack
            (fun
                (element: 'a)
                (soFar:
                    {| Index: int64
                       List: List<struct (int64 * 'a)> |}) ->
                {| Index = soFar.Index - 1L
                   List = (struct (soFar.Index, element)) :: soFar.List |})
            array
            {| Index = int64 (Array.length array - 1)
               List = [] |})
            .List

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
                max 0 (int startInclusivePossiblyNegative + Array.length array)
            else
                int startInclusivePossiblyNegative

        let realEndIndexExclusive: int =
            if (endExclusivePossiblyNegative < 0L) then
                max 0 (int endExclusivePossiblyNegative + Array.length array)
            else
                min (int endExclusivePossiblyNegative) (Array.length array)

        if (realStartIndex >= realEndIndexExclusive) then
            Array.empty
        else
            Array.sub array realStartIndex (realEndIndexExclusive - realStartIndex)


    let JsonEncode_null: System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(null)

    let inline JsonEncode_bool (bool: bool) : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(bool)

    let inline JsonEncode_string
        (string: StringRope)
        : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(StringRope.toString string)

    let inline JsonEncode_int (int: int64) : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(int)

    let inline JsonEncode_float (float: float) : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonValue.Create(float)

    let inline JsonEncode_list
        ([<InlineIfLambda>] elementToValue:
            'element -> System.Text.Json.Nodes.JsonNode)
        (elements: List<'element>)
        : System.Text.Json.Nodes.JsonNode =
        // can be optimized
        System.Text.Json.Nodes.JsonArray(
            Array.ofList (List.map elementToValue elements)
        )

    let inline JsonEncode_array
        ([<InlineIfLambda>] elementToValue:
            'element -> System.Text.Json.Nodes.JsonNode)
        (elements: array<'element>)
        : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonArray(Array.map elementToValue elements)

    let inline JsonEncode_set
        ([<InlineIfLambda>] elementToValue:
            'element -> System.Text.Json.Nodes.JsonNode)
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

    let inline JsonEncode_dict
        ([<InlineIfLambda>] keyToString: 'key -> string)
        ([<InlineIfLambda>] valueToJson: 'value -> System.Text.Json.Nodes.JsonNode)
        (dict: Map<'key, 'value>)
        : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonObject(
            Map.fold
                (fun soFar key value ->
                    Map.add (keyToString key) (valueToJson value) soFar)
                Map.empty
                dict
        )


    let JsonEncode_encode
        (indentDepth: int64)
        (json: System.Text.Json.Nodes.JsonNode)
        : StringRope =
        let printOptions = System.Text.Json.JsonSerializerOptions()

        if (indentDepth <> 0) then
            printOptions.WriteIndented <- true
            printOptions.IndentSize <- int indentDepth

        StringRopeOne(json.ToJsonString(printOptions))

    type JsonDecode_Error =
        | JsonDecode_Field of (struct (StringRope * JsonDecode_Error))
        | JsonDecode_Index of (struct (int64 * JsonDecode_Error))
        | JsonDecode_OneOf of List<JsonDecode_Error>
        | JsonDecode_Failure of
            (struct (StringRope * System.Text.Json.Nodes.JsonNode))

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
            decoder (
                System.Text.Json.Nodes.JsonNode.Parse(StringRope.toString string)
            )
        with :? System.Text.Json.JsonException ->
            Error(
                JsonDecode_Failure(
                    StringRopeOne "This is not valid JSON!",
                    System.Text.Json.Nodes.JsonValue.Create(
                        StringRope.toString string
                    )
                )
            )

    let inline JsonDecode_succeed (value: 'value) : JsonDecode_Decoder<'value> =
        fun _ -> Ok(value)

    let inline JsonDecode_fail
        (errorMessage: StringRope)
        : JsonDecode_Decoder<'value> =
        fun jsonDomNode -> Error(JsonDecode_Failure(errorMessage, jsonDomNode))

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
        fun json -> lazilyConstructDecoder () json

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
        ([<InlineIfLambda>] combine:
            'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'combined)
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
        : JsonDecode_Decoder<option<'value>> =
        fun json ->
            Ok(
                match valueDecoder json with
                | Ok valueDecodeResult -> Some valueDecodeResult
                | Error valueError -> None
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

    let JsonDecode_string: JsonDecode_Decoder<StringRope> =
        fun json ->
            try
                Ok(StringRopeOne(json.AsValue().GetValue<string>()))
            with _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRopeOne "Expecting a STRING", json)
                    )
                )

    let JsonDecode_int: JsonDecode_Decoder<int64> =
        fun json ->
            try
                Ok(json.AsValue().GetValue<int64>())
            with _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRopeOne "Expecting an INT", json)
                    )
                )

    let JsonDecode_float: JsonDecode_Decoder<float> =
        fun json ->
            try
                Ok(json.AsValue().GetValue<float>())
            with _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRopeOne "Expecting a FLOAT", json)
                    )
                )

    let JsonDecode_bool: JsonDecode_Decoder<bool> =
        fun json ->
            try
                Ok(json.AsValue().GetValue<bool>())
            with _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRopeOne "Expecting a BOOL", json)
                    )
                )

    let inline JsonDecode_null (value: 'value) : JsonDecode_Decoder<'value> =
        fun json ->
            match json.GetValueKind() with
            | System.Text.Json.JsonValueKind.Null -> Ok value
            | _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRopeOne "Expecting NULL", json)
                    )
                )

    let JsonDecode_index
        (index: int64)
        (elementDecoder: JsonDecode_Decoder<'element>)
        : JsonDecode_Decoder<'element> =
        fun json ->
            if index <= 0 then
                Error(
                    JsonDecode_Failure(
                        struct (StringRopeOne(
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
                                struct (StringRopeOne(
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
                        | Error error ->
                            Error(JsonDecode_Index(struct (index, error)))
                with _ ->
                    Error(
                        JsonDecode_Failure(
                            struct (StringRopeOne "Expecting an ARRAY", json)
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
                        struct (StringRopeOne "Expecting a LIST", json)
                    )
                )

    let JsonDecode_array
        (elementDecoder: JsonDecode_Decoder<'element>)
        : JsonDecode_Decoder<array<'element>> =
        // can be optimized
        JsonDecode_map Array.ofList (JsonDecode_list elementDecoder)

    let JsonDecode_field
        (fieldNameStringRope: StringRope)
        (valueDecoder: JsonDecode_Decoder<'value>)
        : JsonDecode_Decoder<'value> =
        let fieldNameString = StringRope.toString fieldNameStringRope

        fun json ->
            try
                let jsonObject: System.Text.Json.Nodes.JsonObject = json.AsObject()

                match jsonObject[fieldNameString] with
                | null ->
                    Error(
                        JsonDecode_Failure(
                            struct (StringRopeOne(
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
                        Error(JsonDecode_Field(struct (fieldNameStringRope, error)))
            with _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRopeOne(
                                    "Expecting an OBJECT with a field named '"
                                    + fieldNameString
                                    + "'"
                                ),
                                json)
                    )
                )

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
                        (soFarOrError:
                            Result<Map<StringRope, 'value>, JsonDecode_Error>) ->
                        match soFarOrError with
                        | Error _ -> soFarOrError
                        | Ok soFar ->
                            match valueDecoder field.Value with
                            | Error error ->
                                Error(
                                    JsonDecode_Field(
                                        struct (StringRopeOne field.Key, error)
                                    )
                                )
                            | Ok fieldValue ->
                                Ok(
                                    Map.add
                                        (StringRopeOne field.Key)
                                        fieldValue
                                        soFar
                                ))
                    jsonObject
                    (Ok Map.empty)
            with _ ->
                Error(
                    JsonDecode_Failure(
                        struct (StringRopeOne "Expecting an OBJECT", json)
                    )
                )

    let JsonDecode_keyValuePairs
        (valueDecoder: JsonDecode_Decoder<'value>)
        : JsonDecode_Decoder<List<struct (StringRope * 'value)>> =
        // can be optimized
        JsonDecode_map Dict_toList (JsonDecode_dict valueDecoder)

    let JsonDecode_nullable
        (valueDecoder: JsonDecode_Decoder<'value>)
        : JsonDecode_Decoder<option<'value>> =
        fun json ->
            match JsonDecode_null None json with
            | Ok nullDecodeResult -> Ok nullDecodeResult
            | Error nullError ->
                match valueDecoder json with
                | Ok valueDecodeResult -> Ok(Some valueDecodeResult)
                | Error valueError ->
                    Error(JsonDecode_OneOf [ nullError; valueError ])

    let inline JsonDecoder_oneOrMore
        ([<InlineIfLambda>] combineHeadTail: 'element -> List<'element> -> 'combined)
        (elementDecoder: JsonDecode_Decoder<'element>)
        : JsonDecode_Decoder<'combined> =
        JsonDecode_map2
            combineHeadTail
            elementDecoder
            (JsonDecode_list elementDecoder)

    let inline indent (str: string) : string =
        String.concat "\n    " (Array.toList (str.Split("\n")))

    let rec JsonDecode_errorToStringHelp
        (error: JsonDecode_Error)
        (context: List<string>)
        : string =
        match error with
        | JsonDecode_Field(f, err) ->
            let isSimple =
                match String_uncons f with
                | None -> false
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
        StringRopeOne(JsonDecode_errorToStringHelp error [])


    type Regex_Options =
        { CaseInsensitive: bool
          Multiline: bool }

    type Regex_Match =
        { Match: StringRope
          Index: int64
          Number: int64
          Submatches: List<option<StringRope>> }

    let Regex_fromString
        (string: StringRope)
        : option<System.Text.RegularExpressions.Regex> =
        try
            Some(
                System.Text.RegularExpressions.Regex(
                    StringRope.toString string,
                    System.Text.RegularExpressions.RegexOptions.ECMAScript
                )
            )
        with _ ->
            None

    let Regex_fromStringWith
        (options: Regex_Options)
        (string: StringRope)
        : option<System.Text.RegularExpressions.Regex> =
        try
            Some(
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
            None

    let Regex_never: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex("/.^/")

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
            Array.map StringRopeOne (regex.Split(StringRope.toString string))
        )

    let inline Regex_splitAtMost
        (maxSplitCount: int64)
        (regex: System.Text.RegularExpressions.Regex)
        (string: StringRope)
        : List<StringRope> =
        // can be optimized
        Array.toList (
            Array.map
                StringRopeOne
                (regex.Split(StringRope.toString string, int maxSplitCount))
        )

    let inline regexMatchToRegex_MatchAtIndex0Based
        (matchNumber0Based: int64)
        (regexMatch: System.Text.RegularExpressions.Match)
        : Regex_Match =
        { Match = StringRopeOne regexMatch.Value
          Index = regexMatch.Index
          Number = matchNumber0Based + 1L
          Submatches =
            Seq.toList (
                Seq.map
                    (fun (subMatch: System.Text.RegularExpressions.Group) ->
                        // TODO when does elm return None?
                        Some(StringRopeOne subMatch.Value))
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
            (regex.Matches(string)))
            .Map

    let Regex_replace
        (regex: System.Text.RegularExpressions.Regex)
        (replacementForMatch: Regex_Match -> StringRope)
        (stringRope: StringRope)
        : StringRope =
        let string = StringRope.toString stringRope

        let matchNumbers0Based: Map<string, int64> =
            createRegexMatchNumber0BasedMap regex string

        StringRopeOne(
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
        let string = StringRope.toString stringRope

        let matchNumbers0Based: Map<string, int64> =
            createRegexMatchNumber0BasedMap regex string

        StringRopeOne(
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
        StringRopeOne(value.ToString())

    let inline Debug_todo (message: string) : 'value =
        raise (new System.NotImplementedException(message))


    let ElmKernelParser_isSubString
        (smallStringRope: StringRope)
        (offsetOriginal: int64)
        (rowOriginal: int64)
        (colOriginal: int64)
        (bigStringRope: StringRope)
        : struct (int64 * int64 * int64) =
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
        (predicate: char -> bool)
        (offset: int64)
        (stringRope: StringRope)
        : int64 =
        let string = StringRope.toString stringRope

        if String.length string <= int offset then
            -1
        else if (int (string[int offset]) &&& 0xF800) = 0xD800 then
            (if predicate (char (string.Substring(int offset, 2))) then
                 offset + 2L
             else
                 -1L)
        else if predicate (string[int offset]) then
            (if (string[int offset] = '\n') then -2L else offset + 1L)
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
        StringRopeOne(System.Net.WebUtility.UrlEncode(StringRope.toString string))

    let inline ElmKernelUrl_percentDecode
        (string: StringRope)
        : option<StringRope> =
        match System.Net.WebUtility.UrlDecode(StringRope.toString string) with
        | null -> None
        | decodedString -> Some(StringRopeOne decodedString)


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
                        struct (state,
                                Bitwise_shiftRightZfBy 0L ((|||) 1L ((^^^) b c)))
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
            if (<=) countdown (Basics_fabs weight) then
                value

            else
                Random_getByWeight
                    second
                    otherOthers
                    ((-) countdown (Basics_fabs weight))

    let Random_float (a: float) (b: float) =
        fun (seed0: Random_Seed) ->
            let seed1: Random_Seed = Random_next seed0

            let range: float = Basics_fabs((-) b a)

            let n1: int64 = Random_peel seed1

            let n0: int64 = Random_peel seed0

            let lo: float = (*) (float ((&&&) 134217727L n1)) 1.0

            let hi: float = (*) (float ((&&&) 67108863L n0)) 1.0

            let val_: float = (/) ((+) ((*) hi 134217728.0) lo) 9007199254740992.0

            let scaled: float = (+) ((*) val_ range) a

            (struct (scaled, Random_next seed1))

    let Random_weighted
        (first: (struct (float * 'a)))
        (others: List<(struct (float * 'a))>)
        =
        let normalize ((struct (weight, _)): (struct (float * 'ignored))) =
            Basics_fabs weight

        let total: float =
            (+) (normalize first) (List.sum (List.map normalize others))

        Random_map (Random_getByWeight first others) (Random_float 0.0 total)

    let inline Random_constant (value: 'a) : Random_Generator<'a> =
        fun (seed: Random_Seed) -> (struct (value, seed))

    let Random_andThen
        (callback: 'a -> Random_Generator<'b>)
        (genA: Random_Generator<'a>)
        =
        fun (seed: Random_Seed) ->
            let ((struct (result, newSeed)): (struct ('a * Random_Seed))) =
                genA seed

            let (genB: Random_Generator<'b>) = callback result

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
            Basics_modBy
                7
                (flooredDiv (Time_toAdjustedMinutes zone time) (60L * 24L))
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

    let BytesEncode_unsignedInt8 (u8: int64) : BytesEncode_Encoder =
        BytesEncode_U8 u8

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
        | BytesEncode_I8(_) -> 1
        | BytesEncode_I16(_) -> 2
        | BytesEncode_I32(_) -> 4
        | BytesEncode_U8(_) -> 1
        | BytesEncode_U16(_) -> 2
        | BytesEncode_U32(_) -> 4
        | BytesEncode_F32(_) -> 4
        | BytesEncode_F64(_) -> 8
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
            | BytesEncode_I8(i8) ->
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
            | BytesEncode_U8(u8) ->
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
                        (System.BitConverter.GetBytes(f64))
                )

                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_Utf8(byteLength, stringRope) ->
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
            | BytesEncode_Bytes(byteArray) ->
                mutableBuffer.Write(new System.ReadOnlySpan<byte>(byteArray))

                if mutableRemainingRightEncoders.Count = 0 then
                    shouldKeepGoing <- false
                else
                    toEncodeNext <- mutableRemainingRightEncoders.Pop()
            | BytesEncode_Seq(byteLength, encoders) ->
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
                            mutableRemainingRightEncoders.Push(
                                subEncoderAfterNextSubEncoder
                            ))
                        (Seq.rev subEncodersAfterNextSubEncoder)

        mutableBuffer.ToArray()


    type BytesDecode_Decoder<'value> =
        Bytes_Bytes -> int32 -> ValueOption<struct (int32 * 'value)>

    type BytesDecode_Step<'state, 'a> =
        | BytesDecode_Loop of 'state
        | BytesDecode_Done of 'a

    let BytesDecode_decode
        (decoder: BytesDecode_Decoder<'value>)
        (bytes: Bytes_Bytes)
        : option<'value> =
        match decoder bytes 0 with
        | ValueNone -> None
        | ValueSome(_, value) -> Some value

    let BytesDecode_succeed (value: 'value) : BytesDecode_Decoder<'value> =
        fun bytes index -> ValueSome(index, value)

    let BytesDecode_fail: BytesDecode_Decoder<'value> = fun bytes index -> ValueNone

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
            | ValueSome(indexAfter, value) ->
                ValueSome(indexAfter, valueChange value)

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

                | BytesDecode_Done(result) -> ValueSome(indexAfterStep, result)

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
                | ValueSome(indexAfterB, b) ->
                    ValueSome(indexAfterB, valuesCombine a b)

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
                    match cDecoder bytes indexAfterA with
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
                    match cDecoder bytes indexAfterA with
                    | ValueNone -> ValueNone
                    | ValueSome(indexAfterC, c) ->
                        match dDecoder bytes indexAfterA with
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
                    match cDecoder bytes indexAfterA with
                    | ValueNone -> ValueNone
                    | ValueSome(indexAfterC, c) ->
                        match dDecoder bytes indexAfterA with
                        | ValueNone -> ValueNone
                        | ValueSome(indexAfterD, d) ->
                            match eDecoder bytes indexAfterA with
                            | ValueNone -> ValueNone
                            | ValueSome(indexAfterE, e) ->
                                ValueSome(indexAfterE, valuesCombine a b c d e)

    let BytesDecode_signedInt8: BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 1

            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome(indexAfter, int64 (sbyte (bytes[index])))

    let BytesDecode_signedInt16
        (endianness: Bytes_Endianness)
        : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 2

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
                                .ReadInt16LittleEndian(bytes[index..indexAfter])
                        | Bytes_BE ->
                            System
                                .Buffers
                                .Binary
                                .BinaryPrimitives
                                .ReadInt16BigEndian(bytes[index..indexAfter])
                    )
                )

    let BytesDecode_signedInt32
        (endianness: Bytes_Endianness)
        : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 4

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
                                .ReadInt32LittleEndian(bytes[index..indexAfter])
                        | Bytes_BE ->
                            System
                                .Buffers
                                .Binary
                                .BinaryPrimitives
                                .ReadInt32BigEndian(bytes[index..indexAfter])
                    )
                )

    let BytesDecode_unsignedInt8: BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 1

            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome(indexAfter, int64 (bytes[index]))

    let BytesDecode_unsignedInt16
        (endianness: Bytes_Endianness)
        : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 2

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
                            System
                                .Buffers
                                .Binary
                                .BinaryPrimitives
                                .ReadUInt16BigEndian(bytes[index..indexAfter])
                    )
                )

    let BytesDecode_unsignedInt32
        (endianness: Bytes_Endianness)
        : BytesDecode_Decoder<int64> =
        fun bytes index ->
            let indexAfter = index + 4

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
                            System
                                .Buffers
                                .Binary
                                .BinaryPrimitives
                                .ReadUInt32BigEndian(bytes[index..indexAfter])
                    )
                )

    let BytesDecode_float32
        (endianness: Bytes_Endianness)
        : BytesDecode_Decoder<float> =
        fun bytes index ->
            let indexAfter = index + 4

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
                            System
                                .Buffers
                                .Binary
                                .BinaryPrimitives
                                .ReadSingleBigEndian(bytes[index..indexAfter])
                    )
                )

    let BytesDecode_float64
        (endianness: Bytes_Endianness)
        : BytesDecode_Decoder<float> =
        fun bytes index ->
            let indexAfter = index + 8

            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome(
                    indexAfter,
                    match endianness with
                    | Bytes_LE ->
                        System
                            .Buffers
                            .Binary
                            .BinaryPrimitives
                            .ReadDoubleLittleEndian(bytes[index..indexAfter])
                    | Bytes_BE ->
                        System.Buffers.Binary.BinaryPrimitives.ReadDoubleBigEndian(
                            bytes[index..indexAfter]
                        )
                )

    let BytesDecode_bytes
        (byteCountToRead: int64)
        : BytesDecode_Decoder<Bytes_Bytes> =
        fun bytes index ->
            let indexAfter = index + int byteCountToRead

            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome(indexAfter, bytes[index..indexAfter])

    let BytesDecode_string
        (byteCountToRead: int64)
        : BytesDecode_Decoder<StringRope> =
        fun bytes index ->
            let indexAfter = index + 8

            if indexAfter >= Array.length bytes then
                ValueNone
            else
                ValueSome(
                    indexAfter,
                    StringRopeOne(
                        System.Text.Encoding.UTF8.GetString(
                            bytes,
                            index,
                            int byteCountToRead
                        )
                    )
                )


    let inline MathVector2_vec2 (x: float) (y: float) : System.Numerics.Vector2 =
        System.Numerics.Vector2.Create(float32 x, float32 y)

    let inline MathVector2_getX (vector2: System.Numerics.Vector2) : float =
        float vector2.X

    let inline MathVector2_getY (vector2: System.Numerics.Vector2) : float =
        float vector2.Y

    let inline MathVector2_setX
        (newX: float)
        (vector2: System.Numerics.Vector2)
        : System.Numerics.Vector2 =
        System.Numerics.Vector2.Create(float32 newX, vector2.Y)

    let inline MathVector2_setY
        (newY: float)
        (vector2: System.Numerics.Vector2)
        : System.Numerics.Vector2 =
        System.Numerics.Vector2.Create(vector2.X, float32 newY)

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

    let inline MathVector2_negate
        (vector2: System.Numerics.Vector2)
        : System.Numerics.Vector2 =
        System.Numerics.Vector2.Negate(vector2)

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

    let inline MathVector2_normalize
        (vector2: System.Numerics.Vector2)
        : System.Numerics.Vector2 =
        System.Numerics.Vector2.Normalize(vector2)

    let inline MathVector2_direction
        (a: System.Numerics.Vector2)
        (b: System.Numerics.Vector2)
        : System.Numerics.Vector2 =
        System.Numerics.Vector2.Normalize(System.Numerics.Vector2.Subtract(a, b))

    let inline MathVector2_length (vector2: System.Numerics.Vector2) : float =
        float (vector2.Length())

    let inline MathVector2_lengthSquared
        (vector2: System.Numerics.Vector2)
        : float =
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



    let MathVector3_vec3
        (x: float)
        (y: float)
        (z: float)
        : System.Numerics.Vector3 =
        System.Numerics.Vector3.Create(float32 x, float32 y, float32 z)

    let MathVector3_i: System.Numerics.Vector3 =
        System.Numerics.Vector3.Create(1f, 0f, 0f)

    let MathVector3_j: System.Numerics.Vector3 =
        System.Numerics.Vector3.Create(0f, 1f, 0f)

    let MathVector3_k: System.Numerics.Vector3 =
        System.Numerics.Vector3.Create(0f, 0f, 1f)

    let MathVector3_getX (vector3: System.Numerics.Vector3) : float =
        float vector3.X

    let MathVector3_getY (vector3: System.Numerics.Vector3) : float =
        float vector3.Y

    let MathVector3_getZ (vector3: System.Numerics.Vector3) : float =
        float vector3.Z

    let MathVector3_setX
        (newX: float)
        (vector3: System.Numerics.Vector3)
        : System.Numerics.Vector3 =
        System.Numerics.Vector3.Create(float32 newX, vector3.Y, vector3.Z)

    let MathVector3_setY
        (newY: float)
        (vector3: System.Numerics.Vector3)
        : System.Numerics.Vector3 =
        System.Numerics.Vector3.Create(vector3.X, float32 newY, vector3.Z)

    let MathVector3_setZ
        (newZ: float)
        (vector3: System.Numerics.Vector3)
        : System.Numerics.Vector3 =
        System.Numerics.Vector3.Create(vector3.X, vector3.Y, float32 newZ)

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

    let MathVector3_negate
        (vector3: System.Numerics.Vector3)
        : System.Numerics.Vector3 =
        System.Numerics.Vector3.Negate(vector3)

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

    let MathVector3_normalize
        (vector3: System.Numerics.Vector3)
        : System.Numerics.Vector3 =
        System.Numerics.Vector3.Normalize(vector3)

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
        System.Numerics.Vector4.Create(float32 x, float32 y, float32 z, float32 w)

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
        System.Numerics.Vector4.Create(
            float32 newX,
            vector4.Y,
            vector4.Z,
            vector4.W
        )

    let inline MathVector4_setY
        (newY: float)
        (vector4: System.Numerics.Vector4)
        : System.Numerics.Vector4 =
        System.Numerics.Vector4.Create(
            vector4.X,
            float32 newY,
            vector4.Z,
            vector4.W
        )

    let inline MathVector4_setZ
        (newZ: float)
        (vector4: System.Numerics.Vector4)
        : System.Numerics.Vector4 =
        System.Numerics.Vector4.Create(
            vector4.X,
            vector4.Y,
            float32 newZ,
            vector4.W
        )

    let inline MathVector4_setW
        (newW: float)
        (vector4: System.Numerics.Vector4)
        : System.Numerics.Vector4 =
        System.Numerics.Vector4.Create(
            vector4.X,
            vector4.Y,
            vector4.Z,
            float32 newW
        )

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

    let inline MathVector4_negate
        (vector4: System.Numerics.Vector4)
        : System.Numerics.Vector4 =
        System.Numerics.Vector4.Negate(vector4)

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

    let inline MathVector4_normalize
        (vector4: System.Numerics.Vector4)
        : System.Numerics.Vector4 =
        System.Numerics.Vector4.Normalize(vector4)

    let inline MathVector4_direction
        (a: System.Numerics.Vector4)
        (b: System.Numerics.Vector4)
        : System.Numerics.Vector4 =
        System.Numerics.Vector4.Normalize(System.Numerics.Vector4.Subtract(a, b))

    let inline MathVector4_length (vector4: System.Numerics.Vector4) : float =
        float (vector4.Length())

    let inline MathVector4_lengthSquared
        (vector4: System.Numerics.Vector4)
        : float =
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



    let MathMatrix4_identity: System.Numerics.Matrix4x4 =
        System.Numerics.Matrix4x4.Identity

    let MathMatrix4_inverse
        (matrix4: System.Numerics.Matrix4x4)
        : option<System.Numerics.Matrix4x4> =
        let (wasSuccessful, result) = System.Numerics.Matrix4x4.Invert(matrix4)
        if wasSuccessful then Some(result) else None

    let inline MathMatrix4_mul
        (a: System.Numerics.Matrix4x4)
        (b: System.Numerics.Matrix4x4)
        : System.Numerics.Matrix4x4 =
        System.Numerics.Matrix4x4.Multiply(a, b)

    let inline MathMatrix4_transpose
        (matrix4: System.Numerics.Matrix4x4)
        : System.Numerics.Matrix4x4 =
        System.Numerics.Matrix4x4.Transpose(matrix4)

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
        let left32 = float32 left
        let right32 = float32 right
        let bottom32 = float32 bottom
        let top32 = float32 top
        let zNearPlane32 = float32 zNearPlane
        let zFarPlane32 = float32 zFarPlane

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
            System.Numerics.Matrix4x4.CreateScale(scales)
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
            System.Numerics.Matrix4x4.CreateTranslation(position)
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

    let inline MathMatrix4_makeScale
        (scales: System.Numerics.Vector3)
        : System.Numerics.Matrix4x4 =
        System.Numerics.Matrix4x4.CreateScale(scales)

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

    let inline MathMatrix4_makeTranslate
        (position: System.Numerics.Vector3)
        : System.Numerics.Matrix4x4 =
        System.Numerics.Matrix4x4.CreateTranslation(position)

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
        | PlatformCmd_PortOutgoing of
            PlatformCmd_PortOutgoing: PlatformCmd_PortOutgoing

    type PlatformCmd_Cmd<'event> = List<PlatformCmd_CmdSingle<'event>>

    let PlatformCmd_none: PlatformCmd_Cmd<'event> = []

    let inline PlatformCmd_batch
        (subCommands: List<PlatformCmd_Cmd<'event>>)
        : PlatformCmd_Cmd<'event> =
        List.concat subCommands

    let PlatformCmd_singleMap
        (eventChange: 'event -> 'mappedEvent)
        (commandSingle: PlatformCmd_CmdSingle<'event>)
        : PlatformCmd_CmdSingle<'mappedEvent> =
        match commandSingle with
        | PlatformCmd_PortOutgoing(portOutgoing) ->
            PlatformCmd_PortOutgoing portOutgoing

    let inline PlatformCmd_map
        (eventChange: 'event -> 'mappedEvent)
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
        | PlatformSub_PortIncoming(portIncoming) ->
            PlatformSub_PortIncoming
                { Name = portIncoming.Name
                  OnValue = fun value -> eventChange (portIncoming.OnValue value) }

    let inline PlatformSub_map
        (eventChange: 'event -> 'mappedEvent)
        (subscription: PlatformSub_Sub<'event>)
        : PlatformSub_Sub<'mappedEvent> =
        List.map
            (fun single -> PlatformSub_singleMap eventChange single)
            subscription

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
