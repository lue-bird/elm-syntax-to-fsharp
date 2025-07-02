module GraphTests exposing (suite)

import Expect
import Graph
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Graph"
        [ Test.test "stronglyConnCompR"
            (\_ ->
                Graph.stronglyConnComponents
                    [ ( "a", 0, [ 1 ] )
                    , ( "b", 1, [ 2, 3 ] )
                    , ( "c", 2, [ 1 ] )
                    , ( "d", 3, [ 3 ] )
                    ]
                    |> Expect.equal
                        [ Graph.CyclicSCC [ "d" ]
                        , Graph.CyclicSCC [ "b", "c" ]
                        , Graph.AcyclicSCC "a"
                        ]
            )
        ]
