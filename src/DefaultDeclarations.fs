namespace global

module DefaultDeclarations =
    let inline basics_always (result: 'result) (_: '_ignored) : 'result = result

    let inline basics_eq (a: 'a) (b: 'a) = a = b
    let inline basics_neq (a: 'a) (b: 'a) = a <> b
    let inline basics_lt (a: float) (b: float) : bool = a < b
    let inline basics_le (a: float) (b: float) : bool = a <= b
    let inline basics_gt (a: float) (b: float) : bool = a > b
    let inline basics_ge (a: float) (b: float) : bool = a >= b

    type Basics_Order =
        | Basics_LT
        | Basics_EQ
        | Basics_GT

    let inline basics_compare (a: 'a) (b: 'a) : Basics_Order =
        let comparisonMagnitude = compare a b

        if comparisonMagnitude = 0 then Basics_EQ
        else if comparisonMagnitude < 0 then Basics_LT
        else Basics_GT

    let inline basics_negate (float: float) : float = -float
    let inline basics_add (a: float) (b: float) : float = a + b
    let inline basics_sub (a: float) (b: float) : float = a - b
    let inline basics_mul (a: float) (b: float) : float = a * b
    let inline basics_fdiv (a: float) (b: float) : float = a / b
    let inline basics_idiv (a: float) (b: float) : float = truncate (a / b)
    let inline basics_remainderBy (divisor: float) (toDivide: float) : float =
        toDivide % divisor

    let basics_modBy (divisor: float) (toDivide: float) : float =
        let remainder = toDivide % divisor

        if
            (remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0)
        then
            remainder + toDivide


        else
            remainder

    let inline basics_pow (a: float) (b: float) : float = a ** b

    let inline basics_and (a: bool) (b: bool) : bool = a && b
    let inline basics_or (a: bool) (b: bool) : bool = a || b

    let inline char_isHexDigit (ch : char) : bool =
        System.Char.IsAsciiHexDigit(ch)
    
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
                let mutableRemainingRightStringRopes: ResizeArray<StringRope> = ResizeArray()
                mutableRemainingRightStringRopes.Add(fullRightRope)
                while (shouldKeepGoing) do
                    match stringRopeToMatchNext with
                    | StringRopeOne segment ->
                        let _ = mutableBuilder.Append(segment)
                        if mutableRemainingRightStringRopes.Count = 0 then
                            shouldKeepGoing <- false
                        else
                            stringRopeToMatchNext <-
                                mutableRemainingRightStringRopes.get_Item(mutableRemainingRightStringRopes.Count - 1)
                            mutableRemainingRightStringRopes.RemoveAt(mutableRemainingRightStringRopes.Count - 1)
                    | StringRopeAppend (left, right) ->
                        stringRopeToMatchNext <- left
                        mutableRemainingRightStringRopes.Add(right)
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

    let string_length (str: StringRope) : float =
        float (String.length (StringRope.toString str))

    let string_repeat (repetitions: float) (segment: StringRope) : StringRope =
        StringRopeOne (String.replicate (int repetitions) (StringRope.toString segment))

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

    let string_right (takenElementCount: float) (stringRope: StringRope): StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                String.length string - int takenElementCount,
                int takenElementCount
            ))

    let string_left (skippedElementCount: float) (string: StringRope) : StringRope = 
        StringRopeOne
            ((StringRope.toString string).Substring(0, int skippedElementCount))
    
    let string_dropRight (skippedElementCount: float) (stringRope: StringRope) : StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                0,
                String.length string - int skippedElementCount
            ))

    let string_dropLeft (skippedElementCount: float) (stringRope: StringRope) : StringRope =
        let string : string = StringRope.toString stringRope
        StringRopeOne
            (string.Substring(
                int skippedElementCount,
                String.length string - int skippedElementCount
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
                    .Replace("\r\n", "\n")
                    .Split("\n")
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
        (newMinimumLength: float)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).PadLeft(int newMinimumLength, padding))

    let string_padRight
        (newMinimumLength: float)
        (padding: char)
        (string: StringRope)
        : StringRope =
        StringRopeOne
            ((StringRope.toString string).PadRight(int newMinimumLength, padding))
    
    let string_fromFloat (n: float) : StringRope =
        StringRopeOne (string n)

    let string_toInt (string: StringRope) : option<float> =
        let (success, num) = System.Int64.TryParse (StringRope.toString string)

        if success then Some(float num) else None

    let string_toFloat (string: StringRope) : option<float> =
        let (success, num) = System.Double.TryParse (StringRope.toString string)

        if success then Some(num) else None

    let string_slice
            (startInclusivePossiblyNegative: float)
            (endExclusivePossiblyNegative: float)
            (stringRope: StringRope)
            : StringRope =
        let string = StringRope.toString stringRope
        let realStartIndex: int =
            if (startInclusivePossiblyNegative < 0.0) then
                max
                    0
                    (int startInclusivePossiblyNegative + String.length string)
            else
                int(startInclusivePossiblyNegative)
        let realEndIndexExclusive: int =
            if (endExclusivePossiblyNegative < 0.0) then
                max
                    0
                    (int endExclusivePossiblyNegative + String.length(string))
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

    let list_length (list: List<'a>) =
        float(List.length(list))

    let list_repeat (repetitions: float) (segment: 'a): List<'a> =
        List.replicate (int(repetitions)) segment

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

    let list_product (list: list<float>) : float =
        List.fold basics_mul 1 list

    let inline list_cons (newHead: 'a) (tail: list<'a>) : list<'a> =
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

    let inline list_range (startFloat: float) (endFloat: float) : list<float> =
        [ startFloat..endFloat ]

    let dict_size dict =
        float(Map.count(dict))

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
