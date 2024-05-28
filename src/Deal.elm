module Deal exposing (..)

import Resource exposing (Resource)

type alias Deal =
    { sell : (Resource, Int)
    , buy : (Resource, Int)
    }

value : Deal -> Int
value deal =
    let
        (sellRes, sellCount) = deal.sell
        (buyRes, buyCount) = deal.buy
    in
    buyRes.value * buyCount - sellRes.value * sellCount
