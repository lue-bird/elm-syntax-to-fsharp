namespace global

module DefaultDeclarations =
    let inline Basics_always (result: 'result) (_: '_ignored) : 'result = result

    let inline Basics_eq (a: 'a) (b: 'a) = a = b
    let inline Basics_neq (a: 'a) (b: 'a) = a <> b
    let inline Basics_flt (a: float) (b: float) : bool = a < b
    let inline Basics_ilt (a: int64) (b: int64) : bool = a < b
    let inline Basics_fle (a: float) (b: float) : bool = a <= b
    let inline Basics_ile (a: int64) (b: int64) : bool = a <= b
    let inline Basics_fgt (a: float) (b: float) : bool = a > b
    let inline Basics_igt (a: int64) (b: int64) : bool = a > b
    let inline Basics_fge (a: float) (b: float) : bool = a >= b
    let inline Basics_ige (a: int64) (b: int64) : bool = a >= b

    type Basics_Order =
        | LT = -1
        | EQ = 0
        | GT = 1

    let inline Basics_compare (a: 'a) (b: 'a) : Basics_Order =
        let comparisonMagnitude = compare a b

        if comparisonMagnitude = 0 then Basics_Order.EQ
        else if comparisonMagnitude < 0 then Basics_Order.LT
        else Basics_Order.GT

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
    let Basics_fromPolar (( radius: float, theta: float )) : ( float * float ) =
        ( radius * (System.Double.Cos(theta))
        , radius * (System.Double.Sin(theta))
        )
    let Basics_toPolar ( x: float, y: float ): ( float * float ) =
        ( System.Double.Sqrt((x * x) + (y * y))
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

    let String_length (str: StringRope) : int64 =
        String.length (StringRope.toString str)

    let String_repeat (repetitions: int64) (segment: StringRope) : StringRope =
        StringRopeOne (String.replicate (int repetitions) (StringRope.toString segment))

    let String_toList (string: StringRope) : list<char> =
        List.ofArray ((StringRope.toString string).ToCharArray())

    let String_fromList (chars: list<char>) : StringRope =
        StringRopeOne (new string (List.toArray chars))

    let String_contains (substringRope: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).Contains(StringRope.toString substringRope)

    let String_startsWith (start: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).StartsWith(StringRope.toString start)

    let String_endsWith (ending: StringRope) (string: StringRope) : bool =
        (StringRope.toString string).EndsWith(StringRope.toString ending)

    let String_any
        (charIsNeedle: char -> bool)
        (string: StringRope)
        : bool =
        // can be optimized
        String.exists charIsNeedle (StringRope.toString string)

    let String_all
        (charIsExpected: char -> bool)
        (string: StringRope)
        : bool =
        // can be optimized
        String.forall charIsExpected (StringRope.toString string)

    let String_map
        (charChange: char -> char)
        (string: StringRope)
        : StringRope =
        // can be optimized
        StringRopeOne
            (String.map charChange (StringRope.toString string))

    let String_filter
        (charShouldBeKept: char -> bool)
        (string: StringRope)
        : StringRope =
        // can be optimized
        StringRopeOne
            (String.filter charShouldBeKept (StringRope.toString string))

    let String_foldl
        (reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: StringRope)
        : 'folded =
        // can be optimized
        Array.fold (fun soFar char -> reduce char soFar) initialFolded
            ((StringRope.toString string).ToCharArray())

    let String_foldr
        (reduce: char -> 'folded -> 'folded)
        (initialFolded: 'folded)
        (string: StringRope)
        : 'folded =
        // can be optimized
        Array.foldBack reduce
            ((StringRope.toString string).ToCharArray())
            initialFolded

    let String_trim (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).Trim())
    let String_trimLeft (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).TrimStart())
    let String_trimRight (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).TrimEnd())

    let String_right (takenElementCount: int64) (stringRope: StringRope): StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                String.length string - int takenElementCount,
                int takenElementCount
            ))

    let String_left (skippedElementCount: int64) (string: StringRope) : StringRope = 
        StringRopeOne
            ((StringRope.toString string).Substring(0, int skippedElementCount))
    
    let String_dropRight (skippedElementCount: int64) (stringRope: StringRope) : StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                0,
                String.length string - int skippedElementCount
            ))

    let String_dropLeft (skippedElementCount: int64) (stringRope: StringRope) : StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                int skippedElementCount,
                String.length string - int skippedElementCount
            ))

    let inline String_append (early: StringRope) (late: StringRope) : StringRope =
        StringRopeAppend (early, late)
    let String_fromChar (char: char) : StringRope = StringRopeOne (string char)

    let String_cons (newHeadChar: char) (late: StringRope) : StringRope =
        StringRopeAppend (StringRopeOne (string newHeadChar), late)
    
    let String_uncons (stringRope: StringRope) : option<( char * StringRope )> =
        let string: string = StringRope.toString stringRope
        if System.String.IsNullOrEmpty(string) then
            None
        else
            Some (( string[0], StringRopeOne(string[1..]) ))

    let String_split (separator: StringRope) (string: StringRope) : list<StringRope> =
        List.ofArray
            (Array.map (fun segment -> StringRopeOne segment)
                ((StringRope.toString string).Split(StringRope.toString separator))
            )

    let String_lines (string: StringRope) : list<StringRope> =
        List.ofArray (
            (Array.map (fun line -> StringRopeOne line)
                ((StringRope.toString string)
                    .Replace("\r\n", "\n")
                    .Split("\n")
                )
            )
        )

    let String_reverse (string: StringRope) : StringRope =
        StringRopeOne
            (new string (Array.rev ((StringRope.toString string).ToCharArray())))

    let String_replace
        (toReplace: StringRope)
        (replacement: StringRope)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).Replace(
                StringRope.toString toReplace,
                StringRope.toString replacement
            ))

    let String_toUpper (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).ToUpperInvariant())
    let String_toLower (string: StringRope) : StringRope =
        StringRopeOne ((StringRope.toString string).ToLowerInvariant())

    let String_join (separator: StringRope) (strings: list<StringRope>) : StringRope =
        StringRopeOne
            (String.concat
                (StringRope.toString separator)
                (List.map StringRope.toString strings)
            )
    let String_concat (strings: list<StringRope>) : StringRope =
        StringRopeOne
            (String.concat "" (List.map StringRope.toString strings))

    let String_padLeft
        (newMinimumLength: int64)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).PadLeft(int newMinimumLength, padding))

    let String_padRight
        (newMinimumLength: int64)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).PadRight(int newMinimumLength, padding))
    
    let String_fromFloat (n: float) : StringRope =
        StringRopeOne (string n)
    let String_fromInt (n: int64) : StringRope =
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

    let List_member (needle: 'a) (list: list<'a>) : bool =
        List.exists (fun element -> element = needle) list
    
    let List_minimum (list: List<'a>): option<'a> =
        match list with
        | [] -> None
        | _ :: _ -> Some (List.min list)
    
    let List_maximum (list: List<'a>): option<'a> =
        match list with
        | [] -> None
        | _ :: _ -> Some (List.max list)

    let List_fproduct (list: list<float>) : float =
        List.fold (*) 1.0 list
    let List_iproduct (list: list<int64>) : int64 =
        List.fold (*) 1L list

    let inline List_cons (newHead: 'a) (tail: list<'a>) : list<'a> =
        newHead :: tail
    
    let inline List_repeat (repetitions: int64) (element: 'a) : List<'a> =
        List.replicate (int repetitions) element
    
    let inline List_take (elementCountFromStart: int64) (list: List<'a>) : List<'a> =
        List.truncate (int elementCountFromStart) list
    
    let inline List_drop (skippedElementCountFromStart: int64) (list: List<'a>) : List<'a> =
        try List.skip (int skippedElementCountFromStart) list with
        | :? System.ArgumentOutOfRangeException -> []

    let List_sortWith
        (elementCompare: 'a -> 'a -> Basics_Order)
        (list: List<'a>)
        : List<'a> =
        List.sortWith
            (fun a b -> int (elementCompare a b))
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

    let inline List_range (startFloat: int64) (endFloat: int64) : list<int64> =
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
        (combine: 'a -> 'b -> 'c -> 'd -> 'combined)
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
        (combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined)
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

    let inline Dict_foldr
        (reduce: 'key -> 'value -> 'state -> 'state)
        (initialState: 'state)
        (dict: Map<'key, 'value>)
        =
        Map.foldBack reduce dict initialState

    let inline Dict_foldl
        (reduce: 'key -> 'value -> 'state -> 'state)
        (initialState: 'state)
        (dict: Map<'key, 'value>)
        =
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
        : 'result
        =
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
        (reduce: 'key -> 'state -> 'state)
        (initialState: 'state)
        (set: Set<'key>)
        =
        Set.foldBack reduce set initialState

    let inline Set_foldl
        (reduce: 'key -> 'state -> 'state)
        (initialState: 'state)
        (set: Set<'key>)
        =
        Set.fold (fun soFar k -> reduce k soFar) initialState set
    
    let inline Array_length (array: array<'a>) : int64 =
        Array.length array
    let Array_get (index: int64) (array: array<'element>) : option<'element> =
        Array.tryItem (int index) array
    let inline Array_initialize (count: int64) (indexToElement: int64 -> 'element) : array<'element> =
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
    let inline Array_indexedMap (elementChange: int64 -> 'a -> 'b) (array: array<'a>) : array<'b> =
        Array.mapi (fun index element -> elementChange index element) array
    let Array_toIndexedList (array: array<'a>) : List<( int64 * 'a )> =
        (Array.foldBack
            (fun (element: 'a) (soFar: {| Index: int64; List: List<( int64 * 'a )> |}) ->
                {| Index = soFar.Index - 1L
                ;  List = ( soFar.Index, element ) :: soFar.List
                |}
            )
            array
            {| Index = int64 (Array.length array - 1)
            ;  List = []
            |}
        ).List
    let inline Array_foldl (reduce: 'a -> 'state -> 'state) (initialState: 'state) (array: array<'a>) : 'state =
        Array.fold (fun state element -> reduce element state) initialState array
    let inline Array_foldr (reduce: 'a -> 'state -> 'state) (initialState: 'state) (array: array<'a>) : 'state =
        Array.foldBack reduce array initialState
    let Array_slice (startInclusivePossiblyNegative: int64) (endExclusivePossiblyNegative: int64) (array: array<'a>) : array<'a> =
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
    
    
    type JsonValue =
        | JsonObject of Map<string, JsonValue>
        | JsonArray of List<JsonValue>
        | JsonString of string
        | JsonNumber of float
        | JsonBool of bool
        | JsonNull
    
    let JsonEncode_null : JsonValue = JsonNull
    let inline JsonEncode_bool (bool: bool) : JsonValue = JsonBool bool
    let inline JsonEncode_string (string: StringRope) : JsonValue =
        JsonString (StringRope.toString string)
    let inline JsonEncode_int (int: int64) : JsonValue = JsonNumber (float int)
    let inline JsonEncode_float (float: float) : JsonValue = JsonNumber float
    let inline JsonEncode_list (elementToValue: 'element -> JsonValue) (elements: List<'element>) : JsonValue =
        JsonArray (List.map elementToValue elements)
    let inline JsonEncode_array (elementToValue: 'element -> JsonValue) (elements: array<'element>) : JsonValue =
        // can be optimized
        JsonArray (Array.toList (Array.map elementToValue elements))
    let inline JsonEncode_set (elementToValue: 'element -> JsonValue) (elements: Set<'element>) : JsonValue =
        // can be optimized
        JsonArray (List.map elementToValue (Set.toList elements))
    let inline JsonEncode_object (fields: List<( StringRope * JsonValue )>) : JsonValue =
        JsonObject
            (List.fold
                (fun soFar (fieldName, fieldValue) ->
                    Map.add (StringRope.toString fieldName)
                        fieldValue
                        soFar
                )
                Map.empty
                fields
            )
    let inline JsonEncode_dict
        (keyToString: 'key -> string)
        (valueToJson: 'value -> JsonValue)
        (dict: Map<'key, 'value>)
        : JsonValue =
        JsonObject
            (Map.fold
                (fun soFar key value ->
                    Map.add (keyToString key) (valueToJson value) soFar
                )
                Map.empty
                dict
            )

    let rec jsonValueToNode (json: JsonValue) : System.Text.Json.Nodes.JsonNode =
        match json with
        | JsonNull -> System.Text.Json.Nodes.JsonValue.Create(null)
        | JsonBool(bool) -> System.Text.Json.Nodes.JsonValue.Create(bool)
        | JsonNumber(number) -> System.Text.Json.Nodes.JsonValue.Create(number)
        | JsonString(string) -> System.Text.Json.Nodes.JsonValue.Create(string)
        | JsonArray(elements) ->
            // can be optimized
            System.Text.Json.Nodes.JsonArray(Array.ofList (List.map jsonValueToNode elements))
        | JsonObject(fields) ->
            System.Text.Json.Nodes.JsonObject(
                Map.map (fun _ value -> jsonValueToNode value) fields
            )
    
    let JsonEncode_encode (indentDepth: int64) (json: JsonValue) =
        let printOptions =
            System.Text.Json.JsonSerializerOptions()
        if (indentDepth <> 0) then
            printOptions.WriteIndented <- true
            printOptions.IndentSize <- int indentDepth
        
        (jsonValueToNode json).ToJsonString(printOptions)

    
    let inline Debug_log (tag: StringRope) (value: 'value) : 'value =
        System.Diagnostics.Debug.Print(StringRope.toString tag + ": {0}", value)

        value
    let inline Debug_toString (value: 'value) : StringRope =
        StringRopeOne (value.ToString())
    let inline Debug_todo (message: string) : 'value =
        raise (new System.NotImplementedException(message))

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
