module Graph exposing
    ( Graph, Bounds, Edge, Vertex, Array
    , SCC(..), stronglyConnCompR
    )

{-| An Elm graph type implementation, inspired by Haskell's Data.Graph.

Reference: <https://hackage.haskell.org/package/containers-0.7/docs/src/Data.Graph.html>

This is edited from https://dark.elm.dmy.fr/packages/guida-lang/graph/latest/


@docs Graph, Bounds, Edge, Vertex, Array


## strongly-connected components

@docs SCC, stronglyConnCompR

-}

import FastDict
import FastSet


type alias Array e =
    { bounds : Bounds
    , byIndex : FastDict.Dict Int e
    }


arrayFind : Int -> Array e -> Maybe e
arrayFind i arr =
    FastDict.get i arr.byIndex


arrayAccum : (e -> a -> e) -> e -> Bounds -> List ( Int, a ) -> Array e
arrayAccum f initial bounds ies =
    { bounds = bounds
    , byIndex =
        List.foldl
            (\( i, a ) acc ->
                FastDict.update i
                    (\atIndexOrNothing ->
                        Just (f (atIndexOrNothing |> Maybe.withDefault initial) a)
                    )
                    acc
            )
            (-- FastDict.empty is also possible but not conclusively faster
             fastDictRangeToConstant bounds.min bounds.max initial
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
type SCC vertex
    = AcyclicSCC vertex
    | CyclicSCC (List vertex)


{-| The strongly connected components of a directed graph,
reverse topologically sorted. The function is the same as
'stronglyConnComp', except that all the information about each node retained.
This interface is used when you expect to apply 'SCC' to
(some of) the result of 'SCC', so you don't want to lose the
dependency information.

    stronglyConnCompR
        [ ( "a", 0, [ 1 ] )
        , ( "b", 1, [ 2, 3 ] )
        , ( "c", 2, [ 1 ] )
        , ( "d", 3, [ 3 ] )
        ]
        == [ CyclicSCC [ ( "d", 3, [ 3 ] ) ]
           , CyclicSCC [ ( "b", 1, [ 2, 3 ] ), ( "c", 2, [ 1 ] ) ]
           , AcyclicSCC ( "a", 0, [ 1 ] )
           ]

-}
stronglyConnCompR : List ( node, comparable, List comparable ) -> List (SCC ( node, comparable, List comparable ))
stronglyConnCompR edges0 =
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

                decode : Tree Vertex -> SCC ( node, comparable, List comparable )
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

                                Just vertexKey ->
                                    if mentionsItself v then
                                        CyclicSCC [ vertexKey ]

                                    else
                                        AcyclicSCC vertexKey

                        treeSubsNotEmpty ->
                            let
                                treeSubsSCC : List ( node, comparable, List comparable )
                                treeSubsSCC =
                                    List.foldr dec [] treeSubsNotEmpty
                            in
                            CyclicSCC
                                (case arrayFind v vertexMap of
                                    Nothing ->
                                        treeSubsSCC

                                    Just vKey ->
                                        vKey :: treeSubsSCC
                                )

                dec : Tree Vertex -> List ( node, comparable, List comparable ) -> List ( node, comparable, List comparable )
                dec node vs =
                    -- IGNORE TCO
                    let
                        treeSubsSCC : List ( node, comparable, List comparable )
                        treeSubsSCC =
                            List.foldr dec vs (treeSubs node)
                    in
                    case arrayFind (treeElement node) vertexMap of
                        Nothing ->
                            treeSubsSCC

                        Just elementKey ->
                            elementKey :: treeSubsSCC
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


{-| The bounds of a [`Graph`](#Graph).
-}
type alias Bounds =
    { min : Vertex, max : Vertex }


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
    List.range graph.bounds.min graph.bounds.max


{-| (O(V+E)). Returns the list of edges in the graph.

    edges (buildG (0,-1) []) == []

    edges (buildG (0,2) [(0,1),(1,2)]) == [(0,1),(1,2)]

-}
edges : Graph -> List Edge
edges g =
    List.concatMap
        (\from ->
            case arrayFind from g of
                Nothing ->
                    []

                Just tos ->
                    List.map (\to -> ( from, to )) tos
        )
        (vertices g)


{-| (O(V+E)). Build a graph from a list of edges.

Warning: This function will cause a runtime exception if a vertex in the edge
list is not within the given @Bounds@.

    buildG (0,-1) [] == array (0,-1) []
    buildG (0,2) [(0,1), (1,2)] == array (0,2) [(0,[1]),(1,[2]),(2,[])]
    buildG (0,2) [(0,1), (0,2), (1,2)] == array (0,2) [(0,[2,1]),(1,[2]),(2,[])]

-}
buildG : Bounds -> List Edge -> Graph
buildG boundsOfEdges edgesToBuildFrom =
    arrayAccum
        (\soFar vertex -> vertex :: soFar)
        []
        boundsOfEdges
        edgesToBuildFrom


{-| (O(V+E)). The graph obtained by reversing all edges.

    transposeG (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,[]),(1,[0]),(2,[1])]

-}
transposeG : Graph -> Graph
transposeG g =
    buildG g.bounds (reverseE g)


reverseE : Graph -> List Edge
reverseE g =
    List.map (\( v, w ) -> ( w, v )) (edges g)


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

Warning: The @nodeFromVertex@ function will cause a runtime exception if the
given @Vertex@ does not exist.


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
graphFromEdges edges0 =
    let
        maxVertexIndex : Int
        maxVertexIndex =
            List.length edges0 - 1

        bounds0 : Bounds
        bounds0 =
            { min = 0, max = maxVertexIndex }

        edges1 : List ( Int, ( node, comparable, List comparable ) )
        edges1 =
            List.indexedMap Tuple.pair
                (edges0
                    |> List.sortWith (\( _, k1, _ ) ( _, k2, _ ) -> compare k1 k2)
                )

        keyMap : Array comparable
        keyMap =
            { bounds = bounds0
            , byIndex =
                edges1
                    |> List.foldl
                        (\( v, ( _, k, _ ) ) soFar ->
                            soFar |> FastDict.insert v k
                        )
                        FastDict.empty
            }
    in
    ( { bounds = bounds0
      , byIndex =
            edges1
                |> List.foldl
                    (\( v, ( _, _, ks ) ) soFar ->
                        soFar
                            |> FastDict.insert v
                                (List.filterMap
                                    (\k -> k |> keyToVertexIn keyMap)
                                    ks
                                )
                    )
                    FastDict.empty
      }
    , { bounds = bounds0
      , byIndex = edges1 |> FastDict.fromList
      }
    )


keyToVertexIn : Array comparable -> comparable -> Maybe Vertex
keyToVertexIn keyMap k =
    let
        findVertex : Int -> Int -> Maybe Vertex
        findVertex lo hi =
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
                                findVertex lo (mid - 1)

                            EQ ->
                                Just mid

                            GT ->
                                findVertex (mid + 1) hi
    in
    findVertex 0 keyMap.bounds.max


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
depthFirstSpanningTreeFromVertices graph vs0 =
    let
        go :
            List Vertex
            -> IntSet
            -> List ( Tree Vertex, List Vertex )
            -> List (Tree Vertex)
            -> List (Tree Vertex)
        go vrtcs visited stack acc =
            case vrtcs of
                [] ->
                    case stack of
                        [] ->
                            List.reverse acc

                        ( firstTree, firstVs ) :: ( secondTree, secondVs ) :: rest ->
                            go firstVs
                                visited
                                (( treeAddAsLastSub firstTree secondTree
                                 , secondVs
                                 )
                                    :: rest
                                )
                                acc

                        ( firstTree, firstVs ) :: rest ->
                            go firstVs visited rest (firstTree :: acc)

                v :: vs ->
                    if FastSet.member v visited then
                        go vs visited stack acc

                    else
                        go (Maybe.withDefault [] (arrayFind v graph))
                            (FastSet.insert v visited)
                            (( treeOne v, vs ) :: stack)
                            acc
    in
    go vs0 FastSet.empty [] []


{-| Portable implementation using IntSet.
-}
type alias IntSet =
    FastSet.Set Int


postorder : Tree a -> List a -> List a
postorder node list =
    postorderF (treeSubs node) (treeElement node :: list)


postorderF : List (Tree a) -> List a -> List a
postorderF ts list =
    List.foldr (\node soFar -> soFar |> postorder node)
        list
        ts


postOrd : Graph -> List Vertex
postOrd graph =
    postorderF (depthFirstSpanningTree graph) []


{-| The strongly connected components of a graph, in reverse topological order.

    scc (buildG ( 0, 3 ) [ ( 3, 1 ), ( 1, 2 ), ( 2, 0 ), ( 0, 1 ) ])
        == [ tree 0 [ tree 1 [ tree 2 [] ] ]
           , tree 3 []
           ]

-}
scc : Graph -> List (Tree Vertex)
scc g =
    depthFirstSpanningTreeFromVertices g
        (List.reverse (postOrd (transposeG g)))



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
