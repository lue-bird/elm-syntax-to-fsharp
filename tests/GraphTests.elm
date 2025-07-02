module GraphTests exposing (suite)

import Expect
import Graph
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Graph"
        [ Test.test "stronglyConnCompR"
            (\_ ->
                Graph.stronglyConnCompR
                    [ ( "a", 0, [ 1 ] )
                    , ( "b", 1, [ 2, 3 ] )
                    , ( "c", 2, [ 1 ] )
                    , ( "d", 3, [ 3 ] )
                    ]
                    |> Expect.equal
                        [ Graph.CyclicSCC [ ( "d", 3, [ 3 ] ) ]
                        , Graph.CyclicSCC [ ( "b", 1, [ 2, 3 ] ), ( "c", 2, [ 1 ] ) ]
                        , Graph.AcyclicSCC ( "a", 0, [ 1 ] )
                        ]
            )
        ]
