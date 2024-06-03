module Event exposing (..)

import Dict exposing (Dict)
import Resource exposing (Resource)

type Event
    = MakeResourceViewable Resource
    | MakeResourceClickable Resource
    | StartQuest Quest
    | StartGeneratingDeals
    | AddDeal Int Deal


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


unlockWood : Quest
unlockWood =
    { cost = [ ( Resource.coin, 30 ), ( Resource.wood, 10 ) ]
    , title = "Work gloves"
    , description = "This is your first upgrade. Lets you click for wood"
    , events = [ MakeResourceClickable Resource.wood ]
    }


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
