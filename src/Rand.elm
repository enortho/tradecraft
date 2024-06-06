module Rand exposing (..)

import Dict exposing (Dict)
import Event exposing (Deal)
import Random as R exposing (Generator)
import Resource exposing (Resource)


generateDeal : Resource -> Resource -> List Resource -> Generator Deal
generateDeal first_ second_ rest =
    R.uniform ( first_, second_ ) [ ( second_, first_ ) ]
        |> R.andThen
            (\( first, second ) ->
                R.map2 Tuple.pair
                    (R.uniform first rest)
                    (R.int 1 200)
                    |> R.andThen
                        (\( sellRes, sellCount ) ->
                            R.map2 Tuple.pair
                                (R.uniform second rest)
                                (R.int 1 200)
                                |> R.andThen
                                    (\( buyRes, buyCount ) ->
                                        if sellRes.name == buyRes.name then
                                            generateDeal first_ second_ rest

                                        else
                                            R.constant
                                                { sell = ( sellRes, sellCount )
                                                , buy = ( buyRes, buyCount )
                                                , events = []
                                                }
                                    )
                        )
            )



{- R.map2 Tuple.pair
   (R.uniform first (second::rest))
   (R.int 1 200)
   |> R.andThen
       (\buy ->
           R.map2 Tuple.pair
               (R.uniform first rest)
               (R.int 1 200)
               |> R.map
                   (\sell ->
                       { sell = sell
                       , buy = buy
                       , events = []
                       }
                   )
       )
-}
-- sell resource is randomly picked (from worst resource to best resource unlocked + 1)
-- buy resource is randomly picked from either the best res + 1 or sell res - 3 + randbtwn 0 6
-- sell count is ceil( (randbtwn 0.2 1.2)*amntInInventory  ) (or 1 if this is < 1)
-- buy count is:
--   if you have 0 of it in your inventory, then round(buyresval/sellresval * (randbtwn .5 1.5))


genDeal : { counts : Dict String Int, firstUnlocked : Resource, secondUnlocked : Resource, restUnlocked : List Resource } -> Generator Deal
genDeal { counts, firstUnlocked, secondUnlocked, restUnlocked } =
    let
        maybeNextBestResource : Maybe Resource
        maybeNextBestResource =
            Resource.best firstUnlocked (secondUnlocked :: restUnlocked)
                |> Resource.next

        sellResGenerator : Generator Resource
        sellResGenerator =
            case maybeNextBestResource of
                Nothing ->
                    R.uniform firstUnlocked (secondUnlocked :: restUnlocked)

                Just nextBest ->
                    R.uniform firstUnlocked (secondUnlocked :: nextBest :: restUnlocked)


    in
    sellResGenerator
        |> R.andThen
            (\sellRes ->
                 let
                    andPrev = Maybe.andThen Resource.prev


                    upperResources =
                        case maybeNextBestResource of
                            Nothing ->
                                Resource.resources
                                |> List.filter (\res -> res.value > sellRes.value)

                            Just nextBestRes ->
                                nextBestRes::(Resource.between sellRes nextBestRes)
                    buyResources : List Resource
                    buyResources =
                        [ Just sellRes |> andPrev |> andPrev |> andPrev
                        , Just sellRes |> andPrev |> andPrev
                        , Just sellRes |> andPrev
                        ]
                            |> List.filterMap identity
                            |> (++) upperResources
                 in
            )
