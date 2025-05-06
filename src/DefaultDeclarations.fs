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
    
    let String_uncons (stringRope: StringRope) : option<struct( char * StringRope )> =
        let string: string = StringRope.toString stringRope
        if System.String.IsNullOrEmpty(string) then
            None
        else
            Some (struct( string[0], StringRopeOne(string[1..]) ))

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
    let inline JsonEncode_list (elementToValue: 'element -> System.Text.Json.Nodes.JsonNode) (elements: List<'element>) : System.Text.Json.Nodes.JsonNode =
        // can be optimized
        System.Text.Json.Nodes.JsonArray(Array.ofList (List.map elementToValue elements))
    let inline JsonEncode_array (elementToValue: 'element -> System.Text.Json.Nodes.JsonNode) (elements: array<'element>) : System.Text.Json.Nodes.JsonNode =
        System.Text.Json.Nodes.JsonArray(Array.map elementToValue elements)
    let inline JsonEncode_set (elementToValue: 'element -> System.Text.Json.Nodes.JsonNode) (elements: Set<'element>) : System.Text.Json.Nodes.JsonNode =
        // can be optimized
        System.Text.Json.Nodes.JsonArray(Array.map elementToValue (Set.toArray elements))
    let inline JsonEncode_object (fields: List<struct( StringRope * System.Text.Json.Nodes.JsonNode )>) : System.Text.Json.Nodes.JsonNode =
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
        (keyToString: 'key -> string)
        (valueToJson: 'value -> System.Text.Json.Nodes.JsonNode)
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
            
    
    let JsonEncode_encode (indentDepth: int64) (json: System.Text.Json.Nodes.JsonNode) =
        let printOptions =
            System.Text.Json.JsonSerializerOptions()
        if (indentDepth <> 0) then
            printOptions.WriteIndented <- true
            printOptions.IndentSize <- int indentDepth
        
        json.ToJsonString(printOptions)
    
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
    let inline JsonDecode_map (valueChange: 'a -> 'b) (decoder: JsonDecode_Decoder<'a>) : JsonDecode_Decoder<'b> =
        fun jsonDomNode ->
            match decoder jsonDomNode with
            | Error(error) -> Error(error)
            | Ok(value) -> Ok(valueChange value)
    let JsonDecode_lazy (lazilyConstructDecoder: unit -> JsonDecode_Decoder<'value>) : JsonDecode_Decoder<'value> =
        fun json ->
            lazilyConstructDecoder () json
    let inline JsonDecode_andThen (decoderBasedOnValue: 'a -> JsonDecode_Decoder<'b>) (decoder: JsonDecode_Decoder<'a>) : JsonDecode_Decoder<'b> =
        fun json ->
            match decoder json with
            | Error(error) -> Error(error)
            | Ok(value) -> decoderBasedOnValue value json
    let inline JsonDecode_map2
        (combine: 'a -> 'b -> 'combined)
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
        (combine: 'a -> 'b -> 'c -> 'combined)
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
        (combine: 'a -> 'b -> 'c -> 'd -> 'combined)
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
        (combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'combined)
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
        (combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'combined)
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
        (combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'combined)
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
        (combine: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'combined)
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
    let JsonDecoder_oneOrMore
        (combineHeadTail: 'element -> List<'element> -> 'combined)
        (elementDecoder: JsonDecode_Decoder<'element>)
        : JsonDecode_Decoder<'combined> =
        JsonDecode_map2 combineHeadTail
            elementDecoder
            (JsonDecode_list elementDecoder)

    let inline indent (str: string) : string =
        String.concat "\n    " (Array.toList (str.Split("\n")))
    let rec errorOneOf (i: int) (error: JsonDecode_Error) : string =
        "\n\n(" + string (i + 1) + ") " + indent (errorToString error)
    
    and errorToStringHelp (error: JsonDecode_Error) (context: List<string>) : string =
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
            
            errorToStringHelp err (fieldName :: context)

        | JsonDecode_Index(i, err) ->
            let indexName =
                "[" + string i + "]"
            
            errorToStringHelp err (indexName :: context)

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
                errorToStringHelp err context

            | _ :: _ :: _ ->
                let starter =
                    match context with
                    | [] ->
                        "Json.Decode.oneOf"
                    | _ :: _ ->
                        "The Json.Decode.oneOf at json" + String.concat "" (List.rev context)

                let introduction =
                    starter + " failed in the following " + string (List.length errors) + " ways:"
                
                String.concat "\n\n" (introduction :: List.mapi errorOneOf errors)

        | JsonDecode_Failure(msg, json) ->
            let introduction =
                match context with
                | [] ->
                    "Problem with the given value:\n\n"
                | _ :: _ ->
                    "Problem with the value at json" + String.concat "" (List.rev context) + ":\n\n    "
            
            introduction + indent (JsonEncode_encode 4 json) + "\n\n" + StringRope.toString msg

    and errorToString (error: JsonDecode_Error) : string =
        errorToStringHelp error []
    
    
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
    let inline Regex_replace
        (regex: System.Text.RegularExpressions.Regex)
        (replacementForMatch: Regex_Match -> StringRope)
        (stringRope: StringRope) : StringRope =
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
    let inline Regex_replaceAtMost
        (maxMatchReplacementCount: int64)
        (regex: System.Text.RegularExpressions.Regex)
        (replacementForMatch: Regex_Match -> StringRope)
        (stringRope: StringRope) : StringRope =
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
    

    type Parser_Problem =
        | Parser_Expecting of StringRope
        | Parser_ExpectingInt
        | Parser_ExpectingHex
        | Parser_ExpectingOctal
        | Parser_ExpectingBinary
        | Parser_ExpectingFloat
        | Parser_ExpectingNumber
        | Parser_ExpectingVariable
        | Parser_ExpectingSymbol of StringRope
        | Parser_ExpectingKeyword of StringRope
        | Parser_ExpectingEnd
        | Parser_UnexpectedChar
        | Parser_Problem of StringRope
        | Parser_BadRepeat
