module Board exposing (Board, Player(..), Tile(..)
                      , at, empty, nextPlayer, place
                      , toList, tryPlace
                      , BoardState(..), WinningDirection(..), winner)

import Array exposing (Array)
import Utils.List as LUtils
import Utils.Array as AUtils

type Player
    = X
    | O


players : List Player
players =
    [ X, O ]


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        X ->
            O

        O ->
            X


type Tile
    = Occupied Player
    | Empty


type Board
    = Board (Array (Array Tile))


empty : Board
empty =
    Board <|
        Array.repeat 3 <|
            Array.repeat 3 Empty


at : ( Int, Int ) -> Board -> Tile
at ( rowI, colI ) (Board board) =
    board
        |> Array.get rowI
        |> Maybe.andThen (Array.get colI)
        |> Maybe.withDefault Empty


place : ( Int, Int ) -> Player -> Board -> Board
place ( rowI, colI ) player (Board board) =
    let
        newRow =
            board
                |> Array.get rowI
                |> Maybe.map
                    (Array.indexedMap
                        (\i tile ->
                            if i == colI then
                                Occupied player

                            else
                                tile
                        )
                    )
                |> Maybe.withDefault (Array.repeat 3 Empty)
    in
    board
        |> Array.indexedMap
            (\i row ->
                if i == rowI then
                    newRow

                else
                    row
            )
        |> Board


type BoardState
    = NextPlayer Player
    | Draw
    | Winner Player WinningDirection


type WinningDirection
    = Row Int
    | Column Int
    | Diagonal Int


rows : Board -> Array (Array Tile)
rows (Board board) =
    board

cols : Board -> Array (Array Tile)
cols (Board board) =
    let
        invalidRow =
            Array.repeat 3 Empty

        row1 =
            Array.get 0 board |> Maybe.withDefault invalidRow

        row2 =
            Array.get 1 board |> Maybe.withDefault invalidRow

        row3 =
            Array.get 2 board |> Maybe.withDefault invalidRow
    in
    AUtils.map3
        (\el1 el2 el3 -> Array.empty |> Array.push el1 |> Array.push el2 |> Array.push el3)
        row1
        row2
        row3


diags : Board -> Array (Array Tile)
diags board =
    let
        a00 =
            at ( 0, 0 ) board

        a11 =
            at ( 1, 1 ) board

        a22 =
            at ( 2, 2 ) board

        a02 =
            at ( 0, 2 ) board

        a20 =
            at ( 2, 0 ) board

        d1 =
            Array.empty
                |> Array.push a00
                |> Array.push a11
                |> Array.push a22

        d2 =
            Array.empty
                |> Array.push a02
                |> Array.push a11
                |> Array.push a20
    in
    Array.empty
        |> Array.push d1
        |> Array.push d2


checkWinningArray : Array Tile -> Maybe Player
checkWinningArray tiles =
    players
        |> LUtils.firstWhere
            (\player -> AUtils.all ((==) (Occupied player)) tiles)

checkWinningGroup : Array (Array Tile) -> Maybe ( Player, Int )
checkWinningGroup tileGroup =
    tileGroup
        |> Array.indexedMap
            (\i tiles ->
                checkWinningArray tiles
                    |> Maybe.map (\p -> ( p, i ))
            )
        |> Array.toList
        |> LUtils.firstWhere ((/=) Nothing)
        |> Maybe.andThen identity


checkRowWinner : Board -> Maybe ( Player, WinningDirection )
checkRowWinner = rows >> checkWinningGroup >> Maybe.map (Tuple.mapSecond Row)

checkColWinner : Board -> Maybe ( Player, WinningDirection )
checkColWinner = cols >> checkWinningGroup >> Maybe.map (Tuple.mapSecond Column)

checkDiagWinner : Board -> Maybe ( Player, WinningDirection )
checkDiagWinner = diags >> checkWinningGroup >> Maybe.map (Tuple.mapSecond Diagonal)

isTied : Board -> Bool
isTied = rows >> AUtils.all (AUtils.all ((/=) Empty))

tryPlace : (Int, Int) -> Player -> Board -> (Board, BoardState)
tryPlace (rowI, colI) player board =
    let
        playerAlreadyThere =
            board
            |> at (rowI, colI)
            |> ((/=) Empty)

    in
    if playerAlreadyThere then
        (board, NextPlayer player)
    else
        let
            newBoard : Board
            newBoard =
                board
                    |> place (rowI, colI) player

        in
        case checkRowWinner newBoard of
            Just (winningPlayer, winningRow) ->
                ( newBoard
                , Winner winningPlayer winningRow
                )
            Nothing ->
                case checkColWinner newBoard of
                    Just (winningPlayer, winningCol) ->
                        ( newBoard
                        , Winner winningPlayer winningCol
                        )
                    Nothing ->
                        case checkDiagWinner newBoard of
                            Just (winningPlayer, winningDiag) ->
                                ( newBoard
                                , Winner winningPlayer winningDiag
                                )
                            Nothing ->
                                if isTied newBoard then
                                    ( newBoard
                                    , Draw
                                    )
                                else
                                    ( newBoard
                                    , NextPlayer <| nextPlayer player
                                    )

winner : Board -> Maybe Player
winner board =
    let
        winningArray player arr =
            AUtils.all ((==) (Occupied player)) arr

        winningGroup : Player -> Array (Array Tile) -> Bool
        winningGroup player arrarr =
            AUtils.any (winningArray player) arrarr

        playerWinning player =
            if winningGroup player (rows board) then
                True

            else if winningGroup player (cols board) then
                True

            else if winningGroup player (diags board) then
                True

            else
                False
    in
    if playerWinning X then
        Just X

    else if playerWinning O then
        Just O

    else
        Nothing


toList : Board -> List (List Tile)
toList (Board board) =
    board
        |> Array.map Array.toList
        |> Array.toList
