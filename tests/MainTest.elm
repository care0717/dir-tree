module MainTest exposing (suite)

import Expect exposing (Expectation)
import Main exposing (addChild, deleteChild, resetId)
import MultiwayTree exposing (Tree(..))
import Test exposing (..)


suite : Test
suite =
    describe "Main"
        [ describe "addChild"
            [ test "basic" <|
                \_ -> Tree { id = [], value = "root" } [] |> addChild [] |> Expect.equal (Just (Tree { id = [], value = "root" } [ Tree { id = [ 0 ], value = "" } [] ]))
            , test "complex" <|
                \_ ->
                    Tree { id = [], value = "root" }
                        [ Tree { id = [ 0 ], value = "aaa" } []
                        , Tree { id = [ 1 ], value = "bbb" } []
                        , Tree { id = [ 2 ], value = "ccc" }
                            [ Tree { id = [ 2, 0 ], value = "ccc" } []
                            ]
                        ]
                        |> addChild [ 1 ]
                        |> Expect.equal
                            (Just
                                (Tree { id = [], value = "root" }
                                    [ Tree { id = [ 0 ], value = "aaa" } []
                                    , Tree { id = [ 1 ], value = "bbb" }
                                        [ Tree { id = [ 1, 0 ], value = "" } []
                                        ]
                                    , Tree { id = [ 2 ], value = "ccc" }
                                        [ Tree { id = [ 2, 0 ], value = "ccc" } []
                                        ]
                                    ]
                                )
                            )
            , test "fail" <|
                \_ -> Tree { id = [], value = "root" } [] |> addChild [ 0 ] |> Expect.equal Nothing
            ]
        , describe "deleteChild"
            [ test "basic" <|
                \_ -> Tree { id = [], value = "root" } [ Tree { id = [ 0 ], value = "" } [] ] |> deleteChild [ 0 ] |> Expect.equal (Just (Tree { id = [], value = "root" } []))
            , test "complex" <|
                \_ ->
                    Tree { id = [], value = "root" }
                        [ Tree { id = [ 0 ], value = "aaa" } []
                        , Tree { id = [ 1 ], value = "bbb" } []
                        , Tree { id = [ 2 ], value = "ccc" }
                            [ Tree { id = [ 2, 0 ], value = "ccc" } []
                            ]
                        ]
                        |> deleteChild [ 1 ]
                        |> Expect.equal
                            (Just
                                (Tree { id = [], value = "root" }
                                    [ Tree { id = [ 0 ], value = "aaa" } []
                                    , Tree { id = [ 2 ], value = "ccc" }
                                        [ Tree { id = [ 2, 0 ], value = "ccc" } []
                                        ]
                                    ]
                                )
                            )
            , test "fail" <|
                \_ -> Tree { id = [], value = "root" } [ Tree { id = [ 0 ], value = "" } [] ] |> deleteChild [ 1, 0 ] |> Expect.equal Nothing
            , test "cannot delete root" <|
                \_ -> Tree { id = [], value = "root" } [] |> deleteChild [] |> Expect.equal Nothing
            ]
        , describe "resetId"
            [ test "basic" <|
                \_ ->
                    resetId
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
