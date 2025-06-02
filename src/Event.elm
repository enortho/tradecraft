module Event exposing (..)

import Dict exposing (Dict)
import Resource exposing (Resource)

type Event
    = AddQuest QuestPosition Quest
    | StartGeneratingDeals
    | AddDeal Int Deal
    | SetClickValueForResource String Int
    | AddResourceTrigger ResourceTrigger
    | SetAutoGeneration Resource Float
    | AddDealCapacity Int

type alias ResourceTrigger =
    { resourcesNeeded : List ( Resource, Int )
    , events : List Event
    }


partitionTriggers : Dict String Int -> List ResourceTrigger -> ( List Event, List ResourceTrigger )
partitionTriggers counts triggers =
    let
        ( satisfiedTriggers, unsatisfiedTriggers ) =
            triggers
                |> List.partition
                    (\trigger ->
                        trigger.resourcesNeeded
                            |> List.all
                                (\( res, resourcesNeeded ) ->
                                    counts
                                        |> Dict.get res.name
                                        |> Maybe.withDefault 0
                                        |> (\resCount -> resCount >= resourcesNeeded)
                                )
                    )
    in
    ( List.concatMap .events satisfiedTriggers, unsatisfiedTriggers )


type alias Quest =
    { cost : List ( Resource, Int )
    , title : String
    , description : String
    , events : List Event
    }

type QuestPosition
    = First
    | Second
    | Third


type alias Deal =
    { sell : (Resource, Int)
    , buy : (Resource, Int)
    , events : List Event
    }


dealValue : Deal -> Int
dealValue deal =
    let
        (sellRes, sellCount) = deal.sell
        (buyRes, buyCount) = deal.buy
    in
    buyRes.value * buyCount - sellRes.value * sellCount
