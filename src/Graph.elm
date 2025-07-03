module Graph exposing
    ( Graph, Edge, Vertex, Array
    , SCC(..), stronglyConnComponents
    )

{-| An Elm graph type implementation, inspired by Haskell's Data.Graph.

Reference: <https://hackage.haskell.org/package/containers-0.7/docs/src/Data.Graph.html>

This is edited from https://dark.elm.dmy.fr/packages/guida-lang/graph/latest/


@docs Graph, Edge, Vertex, Array


## strongly-connected components

@docs SCC, stronglyConnComponents

-}

import FastDict
import FastSet


type alias Array element =
    { max : Int
    , byIndex : FastDict.Dict Int element
    }


arrayFind : Int -> Array element -> Maybe element
arrayFind i arr =
    FastDict.get i arr.byIndex


arrayAccum :
    (element -> folded -> element)
    -> element
    -> Int
    -> List ( Int, folded )
    -> Array element
arrayAccum reduce initial max ies =
    { max = max
    , byIndex =
        List.foldl
            (\( index, foldedForIndexSoFar ) soFar ->
                FastDict.update index
                    (\atIndexOrNothing ->
                        Just
                            (reduce
                                (atIndexOrNothing |> Maybe.withDefault initial)
                                foldedForIndexSoFar
                            )
                    )
                    soFar
            )
            (-- FastDict.empty is also possible but not conclusively faster
             fastDictRangeToConstant 0 max initial
            )
            ies
    }


fastDictRangeToConstant : Int -> Int -> a -> FastDict.Dict Int a
fastDictRangeToConstant startInclusive endInclusive constantValue =
    fastDictRangeToConstantInto FastDict.empty
        startInclusive
        endInclusive
        constantValue


fastDictRangeToConstantInto : FastDict.Dict Int a -> Int -> Int -> a -> FastDict.Dict Int a
fastDictRangeToConstantInto list startInclusive endInclusive constantValue =
    if startInclusive <= endInclusive then
        fastDictRangeToConstantInto
            (FastDict.insert endInclusive
                constantValue
                list
            )
            startInclusive
            (endInclusive - 1)
            constantValue

    else
        list


{-| Strongly connected component.
-}
type SCC node
    = AcyclicSCC node
    | CyclicSCC (List node)


{-| The strongly connected components of a directed graph,
reverse topologically sorted. The function is the same as
'stronglyConnComp', except that all the information about each node retained.
This interface is used when you expect to apply 'SCC' to
(some of) the result of 'SCC', so you don't want to lose the
dependency information.

    stronglyConnComponents
        [ ( "a", 0, [ 1 ] )
        , ( "b", 1, [ 2, 3 ] )
        , ( "c", 2, [ 1 ] )
        , ( "d", 3, [ 3 ] )
        ]
        == [ CyclicSCC [ "d" ]
           , CyclicSCC [ "b", "c" ]
           , AcyclicSCC "a"
           ]

-}
stronglyConnComponents : List ( node, comparable, List comparable ) -> List (SCC node)
stronglyConnComponents edges0 =
    case edges0 of
        [] ->
            []

        _ :: _ ->
            let
                ( graph, vertexMap ) =
                    graphFromEdges edges0

                mentionsItself : Int -> Bool
                mentionsItself from =
                    case arrayFind from graph of
                        Nothing ->
                            False

                        Just tos ->
                            List.member from tos

                decode : Tree Vertex -> SCC node
                decode tree =
                    let
                        v : Vertex
                        v =
                            treeElement tree
                    in
                    case treeSubs tree of
                        [] ->
                            case arrayFind v vertexMap of
                                Nothing ->
                                    -- bad state
                                    CyclicSCC []

                                Just ( vertexKeyNode, _, _ ) ->
                                    if mentionsItself v then
                                        CyclicSCC [ vertexKeyNode ]

                                    else
                                        AcyclicSCC vertexKeyNode

                        treeSubsNotEmpty ->
                            let
                                treeSubsSCC : List node
                                treeSubsSCC =
                                    List.foldr dec [] treeSubsNotEmpty
                            in
                            CyclicSCC
                                (case arrayFind v vertexMap of
                                    Nothing ->
                                        treeSubsSCC

                                    Just ( vKeyNode, _, _ ) ->
                                        vKeyNode :: treeSubsSCC
                                )

                dec : Tree Vertex -> List node -> List node
                dec node vs =
                    -- IGNORE TCO
                    let
                        treeSubsSCC : List node
                        treeSubsSCC =
                            List.foldr dec vs (treeSubs node)
                    in
                    case arrayFind (treeElement node) vertexMap of
                        Nothing ->
                            treeSubsSCC

                        Just ( elementKeyNode, _, _ ) ->
                            elementKeyNode :: treeSubsSCC
            in
            List.map decode (scc graph)


{-| Abstract representation of vertices.
-}
type alias Vertex =
    Int


{-| Adjacency list representation of a graph, mapping each vertex to its
list of successors.
-}
type alias Graph =
    Array (List Vertex)


{-| An edge from the first vertex to the second.
-}
type alias Edge =
    ( Vertex, Vertex )


{-| (O(V)). Returns the list of vertices in the graph.

    vertices (buildG (0,-1) []) == []

    vertices (buildG (0,2) [(0,1),(1,2)]) == [0,1,2]

-}
vertices : Graph -> List Vertex
vertices graph =
    List.range 0 graph.max


{-| (O(V+E)). Build a graph from a list of edges.
Each vertex must be at most the first given integer
-}
buildG : Int -> List Edge -> Graph
buildG max edgesToBuildFrom =
    arrayAccum
        (\soFar vertex -> vertex :: soFar)
        []
        max
        edgesToBuildFrom


{-| (O(V+E)). The graph obtained by reversing all edges
-}
transposeG : Graph -> Graph
transposeG g =
    buildG g.max (reverseEdges g)


{-| (O(V+E)). Returns the list of edges in the graph, connections flipped.

    reverseEdges (buildG ( 0, -1 ) []) == []

    reverseEdges (buildG ( 0, 2 ) [ ( 0, 1 ),( 1, 2 ) ]) == [ ( 1, 0 ),( 2, 1 ) ]

-}
reverseEdges : Graph -> List Edge
reverseEdges g =
    List.foldr
        (\from soFar ->
            case arrayFind from g of
                Nothing ->
                    soFar

                Just tos ->
                    List.foldr
                        (\to soFarWithTos ->
                            ( to, from ) :: soFarWithTos
                        )
                        soFar
                        tos
        )
        []
        (vertices g)


{-| (O((V+E) \\log V)). Build a graph from a list of nodes uniquely identified
by keys, with a list of keys of nodes this node should have edges to.

This function takes an adjacency list representing a graph with vertices of
type @key@ labeled by values of type @node@ and produces a @Graph@-based
representation of that list. The @Graph@ result represents the /shape/ of the
graph, and the functions describe a) how to retrieve the label and adjacent
vertices of a given vertex, and b) how to retrieve a vertex given a key.

`(graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList`

  - `graph` is the raw, array based adjacency list for the graph.
  - `nodeFromVertex` returns the node
    associated with the given 0-based int vertex; see /warning/ below. This
    runs in (O(1)) time

To safely use this API you must either extract the list of vertices directly
from the graph or first call @vertexFromKey k@ to check if a vertex
corresponds to the key @k@. Once it is known that a vertex exists you can use
@nodeFromVertex@ to access the labelled node and adjacent vertices. See below
for examples.

Note: The out-list may contain keys that don't correspond to nodes of the
graph; they are ignored.


An empty graph.

    let
        (graph, vertexMap) =
            graphFromEdges []
    in
    graph == array (0,-1) []

A graph where the out-list references unspecified nodes (`'c'`), these are
ignored.

    let
        (graph, _) =
            graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c'])]
    in
    graph == array (0,1) [(0,[1]),(1,[])]

A graph with 3 vertices: ("a") -> ("b") -> ("c")

    let
        (graph, _) =
            graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
    in
    graph == array (0,2) [(0,[1]),(1,[2]),(2,[])]

-}
graphFromEdges :
    List ( node, comparable, List comparable )
    ->
        ( Graph
        , Array ( node, comparable, List comparable )
        )
graphFromEdges edgesUnsorted =
    let
        maxVertexIndex : Int
        maxVertexIndex =
            List.length edgesUnsorted - 1

        edgesSortedIndexed : List ( Int, ( node, comparable, List comparable ) )
        edgesSortedIndexed =
            edgesUnsorted
                |> List.sortWith
                    (\( _, k1, _ ) ( _, k2, _ ) -> compare k1 k2)
                |> List.foldr
                    (\edge soFar ->
                        { index = soFar.index - 1
                        , indexed = ( soFar.index, edge ) :: soFar.indexed
                        }
                    )
                    { index = maxVertexIndex, indexed = [] }
                |> .indexed

        keyMap : Array comparable
        keyMap =
            { max = maxVertexIndex
            , byIndex =
                edgesSortedIndexed
                    |> List.foldl
                        (\( v, ( _, k, _ ) ) soFar ->
                            soFar |> FastDict.insert v k
                        )
                        FastDict.empty
            }
    in
    ( { max = maxVertexIndex
      , byIndex =
            edgesSortedIndexed
                |> List.foldl
                    (\( v, ( _, _, ks ) ) soFar ->
                        soFar
                            |> FastDict.insert v
                                (List.filterMap
                                    (\k -> k |> keyToVertexInArray keyMap)
                                    ks
                                )
                    )
                    FastDict.empty
      }
    , { max = maxVertexIndex
      , byIndex = edgesSortedIndexed |> FastDict.fromList
      }
    )


keyToVertexInArray : Array comparable -> comparable -> Maybe Vertex
keyToVertexInArray keyMap k =
    arrayFindBetween 0 keyMap.max k keyMap


arrayFindBetween : Int -> Int -> comparable -> Array comparable -> Maybe Int
arrayFindBetween lo hi k keyMap =
    if lo > hi then
        Nothing

    else
        let
            mid : Int
            mid =
                lo + (hi - lo) // 2
        in
        case arrayFind mid keyMap of
            Nothing ->
                Nothing

            Just v ->
                case compare k v of
                    LT ->
                        arrayFindBetween lo (mid - 1) k keyMap

                    EQ ->
                        Just mid

                    GT ->
                        arrayFindBetween (mid + 1) hi k keyMap


{-| (O(V+E)). A spanning forest of the graph, obtained from a depth-first
search of the graph starting from each vertex in an unspecified order.
-}
depthFirstSpanningTree : Graph -> List (Tree Vertex)
depthFirstSpanningTree g =
    depthFirstSpanningTreeFromVertices g (vertices g)


{-| (O(V+E)). A spanning forest of the part of the graph reachable from the
listed vertices, obtained from a depth-first search of the graph starting at
each of the listed vertices in order.

This function deviates from King and Launchbury's implementation by
bundling together the functions generate, prune, and chop for efficiency
reasons.

This diverges from the original Haskell implementation by using a stack instead
of `SetM` monadic structure. This allows the function to be non-recursive and
thus more efficient.

-}
depthFirstSpanningTreeFromVertices : Graph -> List Vertex -> List (Tree Vertex)
depthFirstSpanningTreeFromVertices graph fromVertices =
    depthFirstSpanningTreeFromVerticesStep graph fromVertices FastSet.empty [] []


depthFirstSpanningTreeFromVerticesStep :
    Graph
    -> List Vertex
    -> FastSet.Set Vertex
    -> List ( Tree Vertex, List Vertex )
    -> List (Tree Vertex)
    -> List (Tree Vertex)
depthFirstSpanningTreeFromVerticesStep graph fromVertices visited stack soFar =
    case fromVertices of
        [] ->
            case stack of
                [] ->
                    List.reverse soFar

                ( firstTree, firstVs ) :: ( secondTree, secondVs ) :: rest ->
                    depthFirstSpanningTreeFromVerticesStep graph
                        firstVs
                        visited
                        (( treeAddAsLastSub firstTree secondTree
                         , secondVs
                         )
                            :: rest
                        )
                        soFar

                [ ( firstTree, firstVs ) ] ->
                    depthFirstSpanningTreeFromVerticesStep graph
                        firstVs
                        visited
                        []
                        (firstTree :: soFar)

        fromVerticesHead :: fromVerticesTail ->
            if FastSet.member fromVerticesHead visited then
                depthFirstSpanningTreeFromVerticesStep graph
                    fromVerticesTail
                    visited
                    stack
                    soFar

            else
                depthFirstSpanningTreeFromVerticesStep graph
                    (Maybe.withDefault [] (arrayFind fromVerticesHead graph))
                    (FastSet.insert fromVerticesHead visited)
                    (( treeOne fromVerticesHead, fromVerticesTail ) :: stack)
                    soFar


postorderHelp : List (Tree a) -> List a -> List a
postorderHelp remaining soFar =
    -- IGNORE TCO
    List.foldr
        (\node withRemainingSoFar ->
            postorderHelp (treeSubs node) (treeElement node :: withRemainingSoFar)
        )
        soFar
        remaining


postorder : Graph -> List Vertex
postorder graph =
    postorderHelp (depthFirstSpanningTree graph) []


{-| The strongly connected components of a graph, in reverse topological order.

    scc (buildG ( 0, 3 ) [ ( 3, 1 ), ( 1, 2 ), ( 2, 0 ), ( 0, 1 ) ])
        == [ tree 0 [ tree 1 [ tree 2 [] ] ]
           , tree 3 []
           ]

-}
scc : Graph -> List (Tree Vertex)
scc graph =
    depthFirstSpanningTreeFromVertices graph
        (List.reverse (postorder (transposeG graph)))



--


type Tree element
    = Tree element (List (Tree element))


treeOne : element -> Tree element
treeOne onlyElement =
    Tree onlyElement []


treeElement : Tree element -> element
treeElement (Tree innerElement _) =
    innerElement


treeSubs : Tree element -> List (Tree element)
treeSubs (Tree _ innerSubs) =
    innerSubs


treeAddAsLastSub : Tree element -> Tree element -> Tree element
treeAddAsLastSub newLastSub (Tree innerElement innerSubs) =
    Tree innerElement (innerSubs ++ [ newLastSub ])
