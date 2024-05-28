module Event exposing (..)

import Dict exposing (Dict)
import Resource exposing (Resource)


type Event
    = UnlockResource Resource
    | StartQuest Quest
    | StartDeals


type alias ResourceTrigger =
    { resourcesNeeded : List ( Resource, Int )
    , event : Event
    }


checkTriggers : Dict String Int -> List ResourceTrigger -> ( List Event, List ResourceTrigger)
checkTriggers counts triggers =
    let
       (satisfiedTriggers, unsatisfiedTriggers) = triggers
        |> List.partition
            (\trigger ->
                trigger.resourcesNeeded
                    |> List.all
                        (\( res, resourcesNeeded ) ->
                            counts
                                |> Dict.get res.name
                                |> Maybe.withDefault 0
                                |> \resCount -> resCount >= resourcesNeeded
                        )
            )
    in
    ( List.map .event satisfiedTriggers, unsatisfiedTriggers )



type alias Quest =
    { cost : List ( Resource, Int )
    , description : String
    , event : Event
    }


unlockWood : Quest
unlockWood =
    { cost = [ ( Resource.coin, 50 ) ]
    , description = "Unlock wooddddd"
    , event = UnlockResource Resource.wood
    }
