module Tree exposing
    ( Tree(..)
    , element, subs
    , one, addAsLastSub
    )

{-| Recursively branching from an element to multiple sub-trees

@docs Tree
@docs element, subs
@docs one, addAsLastSub

-}


type Tree element
    = Tree element (List (Tree element))


one : element -> Tree element
one onlyElement =
    Tree onlyElement []


element : Tree element -> element
element (Tree innerElement _) =
    innerElement


subs : Tree element -> List (Tree element)
subs (Tree _ innerSubs) =
    innerSubs


addAsLastSub : Tree element -> Tree element -> Tree element
addAsLastSub newLastSub (Tree innerElement innerSubs) =
    Tree innerElement (innerSubs ++ [ newLastSub ])
