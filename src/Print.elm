module Print exposing
    ( Print(..), toString
    , exactly, empty, linebreak, spaceOrLinebreakIndented
    , followedBy, listIntersperseAndFlatten, listMapAndIntersperseAndFlatten
    , withIndentAtNextMultipleOf4, withIndentIncreasedBy, linebreakIndented, emptyOrLinebreakIndented
    , LineSpread(..), lineSpread, lineSpreadMergeWith, lineSpreadListMapAndCombine
    )

{-| simple pretty printing

@docs Print, toString


### primitive

@docs exactly, empty, linebreak, spaceOrLinebreakIndented


### combine

@docs followedBy, listIntersperseAndFlatten, listMapAndIntersperseAndFlatten


### indent

@docs withIndentAtNextMultipleOf4, withIndentIncreasedBy, linebreakIndented, emptyOrLinebreakIndented
@docs LineSpread, lineSpread, lineSpreadMergeWith, lineSpreadListMapAndCombine

-}


{-| Like a string that knows which lines need to be indented
if the whole thing is indented.

For example, elm-format inserts full-on linebreaks inside multi-line strings, between the cases of a case-of
or let declarations of a let-in.
These should never have spaces on them no matter how many levels of indentation in
they are.

-}
type Print
    = Exact String ()
    | FollowedBy Print Print
    | Linebreak () ()
    | LinebreakIndented () ()
    | WithIndentIncreasedBy Int Print
    | WithIndentAtNextMultipleOf4 Print ()


{-| Convert to a String with no extra indentation
and no restrictions on line width
-}
toString : Print -> String
toString print =
    toStringWithIndent 0 print


{-| Convert to a String with given base indentation
-}
toStringWithIndent : Int -> Print -> String
toStringWithIndent indent print =
    toStringWithIndentAndLinebreakIndentAsStringWithRight indent "\n" "" print


toStringWithIndentAndLinebreakIndentAsStringWithRight : Int -> String -> String -> Print -> String
toStringWithIndentAndLinebreakIndentAsStringWithRight indentIgnoringMultiplesOfBy4 linebreakIndentAsString right print =
    -- IGNORE TCO
    case print of
        Exact string () ->
            string ++ right ++ ""

        FollowedBy b a ->
            toStringWithIndentAndLinebreakIndentAsStringWithRight indentIgnoringMultiplesOfBy4
                linebreakIndentAsString
                (toStringWithIndentAndLinebreakIndentAsStringWithRight indentIgnoringMultiplesOfBy4
                    linebreakIndentAsString
                    right
                    b
                )
                a

        Linebreak () () ->
            "\n" ++ right

        LinebreakIndented () () ->
            linebreakIndentAsString ++ right ++ ""

        WithIndentIncreasedBy increase innerPrint ->
            toStringWithIndentAndLinebreakIndentAsStringWithRight
                (indentIgnoringMultiplesOfBy4 + increase + 0)
                (linebreakIndentAsString
                    ++ indentAtMost4 increase
                    ++ ""
                )
                right
                innerPrint

        WithIndentAtNextMultipleOf4 innerPrint () ->
            toStringWithIndentAndLinebreakIndentAsStringWithRight
                0
                (linebreakIndentAsString
                    ++ indentInverseRemainderBy4
                        (indentIgnoringMultiplesOfBy4 - indentIgnoringMultiplesOfBy4 // 4 * 4)
                    ++ ""
                )
                right
                innerPrint


indentAtMost4 : Int -> String
indentAtMost4 atMost4 =
    case atMost4 of
        1 ->
            " "

        2 ->
            "  "

        3 ->
            "   "

        -- 4
        _ ->
            "    "


indentInverseRemainderBy4 : Int -> String
indentInverseRemainderBy4 inverseRemainderBy4 =
    case inverseRemainderBy4 of
        0 ->
            "    "

        1 ->
            "   "

        2 ->
            "  "

        -- 3
        _ ->
            " "


{-| [How many lines](#LineSpread) the given [`Print`](#Print)
take up if turned into a string?
-}
lineSpread : Print -> LineSpread
lineSpread print =
    case print of
        Exact _ () ->
            SingleLine

        FollowedBy b a ->
            lineSpreadWithRemaining a [ b ]

        Linebreak () () ->
            MultipleLines

        LinebreakIndented () () ->
            MultipleLines

        WithIndentIncreasedBy _ innerPrint ->
            lineSpread innerPrint

        WithIndentAtNextMultipleOf4 innerPrint () ->
            lineSpread innerPrint


lineSpreadWithRemaining : Print -> List Print -> LineSpread
lineSpreadWithRemaining print remainingPrints =
    case print of
        Exact _ () ->
            case remainingPrints of
                [] ->
                    SingleLine

                nextPrint :: nextRemainingPrints ->
                    lineSpreadWithRemaining nextPrint nextRemainingPrints

        FollowedBy b a ->
            lineSpreadWithRemaining a (b :: remainingPrints)

        Linebreak () () ->
            MultipleLines

        LinebreakIndented () () ->
            MultipleLines

        WithIndentIncreasedBy _ innerPrint ->
            lineSpreadWithRemaining innerPrint remainingPrints

        WithIndentAtNextMultipleOf4 innerPrint () ->
            lineSpreadWithRemaining innerPrint remainingPrints


{-| A given string. Mostly used for keywords, symbols and literal text.

Do not include linebreaks here and instead use [`linebreak`](#linebreak)

    Print.exactly (4 |> String.fromInt)
        |> Print.toString
    --> "4"

-}
exactly : String -> Print
exactly exactNextString =
    Exact exactNextString ()


{-| `exactly ""`.
Useful when you want to conditionally append something

    printSign : Sign -> Print
    printSign =
        case sign of
            Sign.Negative ->
                Print.exactly "-"

            Sign.Positive ->
                Print.empty

-}
empty : Print
empty =
    exactly ""


{-| `exactly " "`.
Often used after opening brackets
and followed by [`Print.withIndentIncreasedBy 2`](#withIndentIncreasedBy)
-}
space : Print
space =
    exactly " "


{-| Empty line (\\n).
Usually followed by [`Print.linebreakIndented`](#linebreakIndented)
-}
linebreak : Print
linebreak =
    Linebreak () ()


{-| Prepend a given [`Print`](#Print)

    Print.exactly "a"
        |> Print.followedBy (Print.exactly "b")
        |> Print.toString
    --> "ab"

To append more than 2, use helpers like [`Print.listIntersperseAndFlatten`](#listIntersperseAndFlatten)

-}
followedBy : Print -> Print -> Print
followedBy =
    FollowedBy


{-| Concatenate a given list of [`Print`](#Print)s
one after the other

    [ "a", "b" ]
        |> List.map Print.exactly
        |> Print.listIntersperseAndFlatten (Print.exactly ",")
        |> Print.toString
    --> "a,b"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
listIntersperseAndFlatten : Print -> List Print -> Print
listIntersperseAndFlatten inBetweenPrint elements =
    case elements of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar ->
                        soFar
                            |> followedBy inBetweenPrint
                            |> followedBy next
                    )
                    head


{-| Concatenate a given list of [`Print`](#Print)s
one after the other

    [ "a", "b" ]
        |> Print.listMapAndIntersperseAndFlatten
            Print.exactly
            (Print.exactly ",")
        |> Print.toString
    --> "a,b"

To only concatenate 2, use [`Print.followedBy`](#followedBy)

-}
listMapAndIntersperseAndFlatten : (a -> Print) -> Print -> List a -> Print
listMapAndIntersperseAndFlatten elementToPrint inBetweenPrint prints =
    case prints of
        [] ->
            empty

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar ->
                        soFar
                            |> followedBy inBetweenPrint
                            |> followedBy (elementToPrint next)
                    )
                    (elementToPrint head)


{-| Set the indentation used by [`Print.linebreakIndented`](#linebreakIndented),
[`Print.spaceOrLinebreakIndented`](#spaceOrLinebreakIndented)
and [`Print.emptyOrLinebreakIndented`](#emptyOrLinebreakIndented)
to the current indent + a given number.
-}
withIndentIncreasedBy : Int -> Print -> Print
withIndentIncreasedBy =
    WithIndentIncreasedBy


{-| Set the indentation used by [`Print.linebreakIndented`](#linebreakIndented),
[`Print.spaceOrLinebreakIndented`](#spaceOrLinebreakIndented)
and [`Print.emptyOrLinebreakIndented`](#emptyOrLinebreakIndented)
to the current indent minus its remainder by 4 + 4.
-}
withIndentAtNextMultipleOf4 : Print -> Print
withIndentAtNextMultipleOf4 print =
    WithIndentAtNextMultipleOf4 print ()


{-| All on the same line or split across multiple?
-}
type LineSpread
    = SingleLine
    | MultipleLines


{-| If either spans [`MultipleLines`](#LineSpread), gives [`MultipleLines`](#LineSpread).
If both are [`SingleLine`](#LineSpread), gives [`SingleLine`](#LineSpread).

To merge a list, use [`Print.lineSpreadListMapAndCombine`](#lineSpreadListMapAndCombine)

-}
lineSpreadMergeWith : (() -> LineSpread) -> LineSpread -> LineSpread
lineSpreadMergeWith bLineSpreadLazy aLineSpread =
    case aLineSpread of
        MultipleLines ->
            MultipleLines

        SingleLine ->
            bLineSpreadLazy ()


{-| If any element spans [`MultipleLines`](#LineSpread), gives [`MultipleLines`](#LineSpread).
If all are [`SingleLine`](#LineSpread), gives [`SingleLine`](#LineSpread).

To only combine 2, use [`Print.lineSpreadMergeWith`](#lineSpreadMergeWith)

-}
lineSpreadListMapAndCombine : (a -> LineSpread) -> List a -> LineSpread
lineSpreadListMapAndCombine elementLineSpread lineSpreads =
    case lineSpreads of
        [] ->
            SingleLine

        head :: tail ->
            case elementLineSpread head of
                MultipleLines ->
                    MultipleLines

                SingleLine ->
                    lineSpreadListMapAndCombine elementLineSpread tail


{-| `Print.exactly " "` when [`SingleLine`](#LineSpread),
[`Print.linebreakIndented`](#linebreakIndented) when [`MultipleLines`](#LineSpread),

Specify the indentation with
[`Print.withIndentIncreasedBy`](#withIndentIncreasedBy)
and [`withIndentAtNextMultipleOf4`](#withIndentAtNextMultipleOf4)

-}
spaceOrLinebreakIndented : LineSpread -> Print
spaceOrLinebreakIndented lineSpreadToUse =
    case lineSpreadToUse of
        SingleLine ->
            space

        MultipleLines ->
            linebreakIndented


{-| [`Print.empty`](#empty) when [`SingleLine`](#LineSpread),
[`Print.linebreakIndented`](#linebreakIndented) when [`MultipleLines`](#LineSpread),

Specify the indentation with
[`Print.withIndentIncreasedBy`](#withIndentIncreasedBy)
and [`withIndentAtNextMultipleOf4`](#withIndentAtNextMultipleOf4)

-}
emptyOrLinebreakIndented : LineSpread -> Print
emptyOrLinebreakIndented lineSpreadToUse =
    case lineSpreadToUse of
        SingleLine ->
            empty

        MultipleLines ->
            linebreakIndented


{-| Linebreak followed by spaces to the current indentation level.

Specify the indentation with
[`Print.withIndentIncreasedBy`](#withIndentIncreasedBy)
and [`withIndentAtNextMultipleOf4`](#withIndentAtNextMultipleOf4)

-}
linebreakIndented : Print
linebreakIndented =
    LinebreakIndented () ()
