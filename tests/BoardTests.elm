module BoardTests exposing (..)

import Test exposing (Test)
import Test exposing (describe, test)
import Expect
import Board exposing (Tile(..), Player(..))

suite : Test
suite =
    describe "The Board Module"
        [ describe "Board.empty"
              [ test "is empty" <|
                    \_ ->
                      Board.empty
                        |> Board.toList
                        |> Expect.equal [ [ Empty, Board.Empty, Board.Empty ]
                                        , [ Board.Empty, Board.Empty, Board.Empty ]
                                        , [ Board.Empty, Board.Empty, Board.Empty ]
                                        ]
              , test "is equal to itself" <|
                    \_ ->
                        Board.empty |> Expect.equal Board.empty
              , test "is not winnng" <|
                  \_ ->
                      Board.empty |> Board.winner |> Expect.equal Nothing
              ]
        , describe "Board.place"
            [ test "first position placed" <|
                  \_ ->
                    Board.empty
                      |> Board.place (0,0) Board.X
                      |> Board.toList
                      |> Expect.equal [ [ Board.Occupied Board.X, Board.Empty, Board.Empty ]
                                      , [ Board.Empty, Board.Empty, Board.Empty ]
                                      , [ Board.Empty, Board.Empty, Board.Empty  ]
                                      ]
            , test "last position placed" <|
                \_ ->
                    Board.empty
                        |> Board.place (2,2) O
                        |> Board.toList
                        |> Expect.equal [ [ Empty, Empty, Empty ]
                                        , [ Empty, Empty, Empty ]
                                        , [ Empty, Empty, Occupied O ]
                                        ]
            ]
        , describe "Board.at"
            [ describe "Empty board"
                [ test "0,0" <|
                    \_ ->
                      Board.empty
                        |> Board.at (0,0)
                        |> Expect.equal Empty
                ]
            , test "placed in the middle" <|
                \_ ->
                    Board.empty
                        |> Board.place (1,1) O
                        |> Board.at (1,1)
                        |> Expect.equal (Occupied O)
            ]

        ]
