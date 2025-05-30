module Rand exposing (..)

import Array exposing (Array)
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


genDeal : { counts : Dict String Int } -> Generator Deal
genDeal { counts } =
    let
        indexedFirstWhere : (a -> Bool) -> Array a -> Maybe ( Int, a )
        indexedFirstWhere fn array =
            indexedFirstWhereHelp fn array 0 (Array.length array)

        indexedFirstWhereHelp : (a -> Bool) -> Array a -> Int -> Int -> Maybe ( Int, a )
        indexedFirstWhereHelp fn array i n =
            if i >= n then
                Nothing

            else
                case Array.get i array of
                    Nothing ->
                        indexedFirstWhereHelp fn array (i + 1) n

                    Just val ->
                        if fn val then
                            Just <| ( i, val )

                        else
                            indexedFirstWhereHelp fn array (i + 1) n

        last : List a -> Maybe a
        last list =
            case list of
                [] ->
                    Nothing

                [ a ] ->
                    Just a

                first :: rest ->
                    last rest

        bestResource : Resource
        bestResource =
            (Resource.firstResource :: Resource.restResources)
                |> List.filterMap (\res -> counts |> Dict.get res.name |> Maybe.map (\count -> ( res, count )))
                |> last
                |> Maybe.map Tuple.first
                |> Maybe.withDefault Resource.coin

        bestResIndex : Int
        bestResIndex =
            Resource.resourceArray
                |> indexedFirstWhere (\res -> res.value == bestResource.value)
                |> Maybe.map Tuple.first
                |> Maybe.withDefault (Resource.numResources - 1)

        nextBestIndex =
            Resource.resourceArray
                |> indexedFirstWhere (\res -> res.value > bestResource.value)
                |> Maybe.map Tuple.first

        sellIndexGenerator =
            case nextBestIndex of
                Nothing ->
                    R.int 0 (Array.length Resource.resourceArray - 1)

                Just index ->
                    R.int 0 index
    in
    sellIndexGenerator
        |> R.andThen
            (\sellIndex ->
                R.int 0 6
                    |> R.andThen
                        (\randomTreat ->
                            let
                                buyIndex =
                                    max 0 <|
                                        min (min (nextBestIndex |> Maybe.withDefault (Array.length Resource.resourceArray - 1)) (sellIndex - 3 + randomTreat)) (Resource.numResources - 1)
                            in
                            if buyIndex == sellIndex then
                                genDeal { counts = counts }

                            else
                                let
                                    sellResource =
                                        Resource.resourceArray
                                            |> Array.get sellIndex
                                            |> Maybe.withDefault Resource.coin

                                    buyResource =
                                        Resource.resourceArray
                                            |> Array.get buyIndex
                                            |> Maybe.withDefault Resource.coin
                                in
                                R.float 0.2 1.2
                                    |> R.andThen
                                        (\sellvalrand ->
                                            let
                                                sellResAmount =
                                                    counts
                                                        |> Dict.get sellResource.name
                                                        |> Maybe.withDefault 0

                                                sellCount =
                                                    min ((globalCap + 1) // 10 - 1) <|
                                                        max 1 <|
                                                            ceiling <|
                                                                sellvalrand
                                                                    * toFloat sellResAmount

                                                sellTotalValue =
                                                    sellCount * sellResource.value
                                            in
                                            R.float 0 1
                                                |> R.andThen
                                                    (\buyvalrand ->
                                                        let
                                                            buyInitCount : Int
                                                            buyInitCount =
                                                                round <|
                                                                    min ((globalCap + 1) / 10 - 1) <|
                                                                        toFloat <|
                                                                            round <|
                                                                                (toFloat sellTotalValue / toFloat buyResource.value)
                                                                                    * (tradeMinRatio + (tradeMaxRatio - tradeMinRatio) * buyvalrand)
                                                        in
                                                        if buyInitCount /= 0 then
                                                            R.constant
                                                                { sell = ( sellResource, sellCount )
                                                                , buy = ( buyResource, buyInitCount )
                                                                , events = []
                                                                }

                                                        else
                                                            R.float 0.5 1.5
                                                                |> R.andThen
                                                                    (\lastrandomvalueipromise ->
                                                                        let
                                                                            actualSellCountWeAreUsing : Int
                                                                            actualSellCountWeAreUsing =
                                                                                round <|
                                                                                    toFloat buyResource.value
                                                                                        / toFloat sellResource.value
                                                                                        * lastrandomvalueipromise
                                                                        in
                                                                        R.constant
                                                                            { sell = ( sellResource, actualSellCountWeAreUsing )
                                                                            , buy = ( buyResource, 1 )
                                                                            , events = []
                                                                            }
                                                                    )
                                                    )
                                        )
                        )
            )


globalCap =
    99999


tradeMinRatio =
    0.5


tradeMaxRatio =
    1.5



{-
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
-}
