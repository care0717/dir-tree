module MainTest exposing (suite)

import Expect exposing (Expectation)
import Main exposing (addChild, deleteChild, resetRootId)
import MultiwayTree exposing (Tree(..))
import Test exposing (..)


suite : Test
suite =
    describe "Main"
        [ describe "resetRootId"
            [ test "basic" <|
                \_ ->
                    resetRootId
                        (Tree { id = [], value = "root" }
                            [ Tree { id = [ 1 ], value = "aaa" } [ Tree { id = [ 2, 1000 ], value = "aa" } [] ]
                            , Tree { id = [ 3 ], value = "ccc" }
                                [ Tree { id = [ 5, 1 ], value = "ccc" } []
                                ]
                            ]
                        )
                        |> Expect.equal
                            (Tree { id = [], value = "root" }
                                [ Tree { id = [ 0 ], value = "aaa" } [ Tree { id = [ 0, 0 ], value = "aa" } [] ]
                                , Tree { id = [ 1 ], value = "ccc" }
                                    [ Tree { id = [ 1, 0 ], value = "ccc" } []
                                    ]
                                ]
                            )
            ]
        ]
