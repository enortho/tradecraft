module Board exposing (Board, Player(..), Tile(..), at, empty, nextPlayer, place, toList, tryPlace, winner)

import Array exposing (Array)


type Player
    = X
    | O


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


tryPlace : ( Int, Int ) -> Player -> Board -> (Maybe Board, Maybe Player)
tryPlace ( rowI, colI ) player board =
    if at ( rowI, colI ) board /= Empty then
        ( Nothing, Nothing )
    else
        let
            b =
                case board of
                    Board bo ->
                        bo

            newRow =
                b
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
            newBoard = Maybe.map (\row -> Array.set rowI row b |> Board) newRow
        in
        ( newBoard, Maybe.andThen winner newBoard )


rows : Board -> Array (Array Tile)
rows (Board board) =
    board


amap3 : (a -> a -> a -> b) -> Array a -> Array a -> Array a -> Array b
amap3 fn a1 a2 a3 =
    let
        acc fn_ a1_ a2_ a3_ i acc_ =
            case
                Maybe.map3 fn_
                    (Array.get i a1)
                    (Array.get i a2)
                    (Array.get i a3)
            of
                Nothing ->
                    acc_

                Just val ->
                    acc fn_ a1_ a2_ a3_ (i + 1) (Array.push val acc_)
    in
    acc fn a1 a2 a3 0 Array.empty


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
    amap3
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


aany : (a -> Bool) -> Array a -> Bool
aany fn array =
    let
        acc fn_ array_ i =
            case Array.get i array_ of
                Nothing ->
                    False

                Just val ->
                    if fn_ val then
                        True

                    else
                        acc fn_ array_ (i + 1)
    in
    acc fn array 0


aall : (a -> Bool) -> Array a -> Bool
aall fn array =
    let
        acc fn_ array_ i =
            case Array.get i array_ of
                Nothing ->
                    True

                Just val ->
                    if fn val then
                        acc fn_ array_ (i + 1)

                    else
                        False
    in
    acc fn array 0


winner : Board -> Maybe Player
winner board =
    let
        winningArray player arr =
            aall ((==) (Occupied player)) arr

        winningGroup : Player -> Array (Array Tile) -> Bool
        winningGroup player arrarr =
            aany (winningArray player) arrarr

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
