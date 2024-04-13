module UtilsList exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Utils.List exposing (firstWhere, firstWhereIndexed)


suite : Test
suite =
    describe "Utils.List tests"
        [ describe "Utils.List.firstWhereIndexed"
            [ test "a" <|
                \_ ->
                    [ True, True, True ]
                        |> firstWhereIndexed identity
                        |> Expect.equal (Just ( True, 0 ))
            , test "b" <|
                \_ ->
                    [ 1, 2, 3, 4, 5, 6 ]
                        |> firstWhereIndexed (\x -> x > 3)
                        |> Expect.equal (Just ( 4, 3 ))
            ]
        ]
