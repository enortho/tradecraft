module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Event exposing (Deal, Event, Quest)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Platform.Cmd as Cmd
import Process
import Rand
import Random
import Resource exposing (Resource)
import Task


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { seed : Random.Seed
    , unlockedResources : List Resource
    , manualScore : Int
    , tradingScore : Int
    , autoScore : Int
    , triggers : List Event.ResourceTrigger
    , counts : Dict String Int
    , clickValues : Dict String Int
    , deals : Dict Int Deal
    , generateDeals : Bool
    , maxDeals : Int
    , quests : List Quest
    }


type Msg
    = Noop
    | ResourceClicked Resource
    | DealTaken Int Deal
    | QuestFulfilled Quest
    | RandomDealDelayReceived Float
    | NewDealGenerated { deal : Deal, position : Int }


init : Int -> ( Model, Cmd Msg )
init seedInit =
    ( { seed = Random.initialSeed seedInit
      , unlockedResources = [ Resource.coin ]
      , manualScore = 0
      , tradingScore = 0
      , autoScore = 0
      , counts = Dict.empty
      , clickValues = Dict.empty |> Dict.insert Resource.coin.name 1
      , triggers = [ firstDealTrigger ]
      , deals = Dict.empty
      , maxDeals = 4
      , generateDeals = False
      , quests = []
      }
    , Cmd.none
    )


firstDealTrigger : Event.ResourceTrigger
firstDealTrigger =
    { resourcesNeeded = [ ( Resource.coin, 30 ) ]
    , events =
        [ Event.AddDeal
            0
            { sell = ( Resource.coin, 50 )
            , buy = ( Resource.wood, 35 )
            , events =
                [ Event.UnlockResource Resource.wood
                ]
            }
        ]
    }


after : Float -> msg -> Cmd msg
after ms msg =
    Process.sleep ms |> Task.perform (always msg)


applyEvents : List Event -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyEvents events modelcmd =
    let
        handleEvent : Event.Event -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
        handleEvent event ( model, cmdmsg ) =
            case event of
                Event.StartGeneratingDeals ->
                    ( model
                    , Cmd.batch [ cmdmsg, genDealAfterDelay ]
                    )

                Event.UnlockResource res ->
                    ( { model
                        | unlockedResources = model.unlockedResources ++ [ res ]
                      }
                    , cmdmsg
                    )

                Event.StartQuest quest ->
                    ( { model
                        | quests = model.quests ++ [ quest ]
                      }
                    , cmdmsg
                    )

                Event.AddDeal index deal ->
                    ( { model
                        | deals = model.deals |> Dict.insert index deal
                      }
                    , cmdmsg
                    )

                Event.SetClickValueForResource resName newClickVal ->
                    ( { model | clickValues = model.clickValues |> Dict.insert resName newClickVal
                      }
                    , cmdmsg
                    )
    in
    List.foldl handleEvent modelcmd events


checkResourceTriggers : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkResourceTriggers ( model, cmdmsg ) =
    let
        ( eventsFromTriggers, unsatisfiedTriggers ) =
            model.triggers
                |> Event.partitionTriggers model.counts
    in
    let
        ( newModel, newCmd ) =
            applyEvents eventsFromTriggers ( model, cmdmsg )
    in
    ( { newModel | triggers = unsatisfiedTriggers }
    , Cmd.batch [ cmdmsg, newCmd ]
    )


genDealAfterDelay : Cmd Msg
genDealAfterDelay =
    Random.float 1000 7000
        |> Random.generate RandomDealDelayReceived


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ResourceClicked res ->
            let
                currentCount =
                    model.counts
                        |> Dict.get res.name
                        |> Maybe.withDefault 0

                clickValue =
                    model.clickValues
                        |> Dict.get res.name
                        |> Maybe.withDefault 0

                totalValueGenerated = clickValue * res.value
            in
            ( { model
                | counts = model.counts |> Dict.insert res.name (currentCount + clickValue)
                , manualScore = model.manualScore + totalValueGenerated
              }
            , Cmd.none
            )
                |> checkResourceTriggers

        DealTaken index deal ->
            let
                ( sellResource, sellResourceCount ) =
                    deal.sell

                ( buyResource, buyResourceCount ) =
                    deal.buy

                sellResourceCountInInventory =
                    model.counts
                        |> Dict.get sellResource.name
                        |> Maybe.withDefault 0
            in
            -- need to make the trade in the deal
            ( { model
                | counts =
                    let
                        buyResourceCountInInventory =
                            model.counts
                                |> Dict.get buyResource.name
                                |> Maybe.withDefault 0
                    in
                    model.counts
                        |> Dict.insert sellResource.name (sellResourceCountInInventory - sellResourceCount)
                        |> Dict.insert buyResource.name (buyResourceCountInInventory + buyResourceCount)
                , tradingScore = model.tradingScore + Event.dealValue deal
                , deals = model.deals |> Dict.remove index
              }
            , Cmd.none
            )
                |> checkResourceTriggers
                |> applyEvents deal.events

        QuestFulfilled quest ->
            let
                newCounts =
                    quest.cost
                        |> List.foldl
                            (\( res, resCount ) counts ->
                                let
                                    resCountInInventory =
                                        counts |> Dict.get res.name |> Maybe.withDefault 0
                                in
                                counts
                                    |> Dict.insert res.name (resCountInInventory - resCount)
                            )
                            model.counts
            in
            ( { model
                | counts = newCounts
                , quests = model.quests |> List.filter ((/=) quest)
              }
            , Cmd.none
            )
                |> applyEvents quest.events

        NewDealGenerated { deal, position } ->
            ( { model
                | deals = model.deals |> Dict.insert position deal
              }
            , genDealAfterDelay
            )

        RandomDealDelayReceived timeUntilNextDealGenerated ->
            let
                generate =
                    Random.map2 (\deal position -> { deal = deal, position = position })
                        (Rand.genDeal { counts = model.counts })
                        (Random.int 0 (model.maxDeals - 1))

                ( newDealInfo, seed0 ) =
                    Random.step generate model.seed
            in
            ( { model | seed = seed0 }
            , after timeUntilNextDealGenerated (NewDealGenerated newDealInfo)
            )


subscriptions : Model -> Sub Msg
subscriptions = always Sub.none


margin0 : H.Attribute msg
margin0 =
    A.style "margin" "0"


view : Model -> Html Msg
view model =
    H.div
        [ A.style "display" "grid"
        , A.style "grid-template-columns" "1fr 1fr"
        ]
        [ H.div
            [ A.style "display" "flex"
            , A.style "flex-direction" "column"
            , A.style "align-items" "center"
            ]
            [ viewDeals model
            , viewResources model
            , viewScoreBar model
            ]
        , viewQuests model.counts model.quests
        ]


viewDeals : { r | counts : Dict String Int, clickValues : Dict String Int, deals : Dict Int Deal } -> Html Msg
viewDeals { counts, deals } =
    let
        greatestIndex =
            Debug.log "deals" deals
                |> Dict.toList
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0

        htmlDeals =
            List.range 0 greatestIndex
                |> List.map
                    (\index ->
                        case Dict.get index deals of
                            Just deal ->
                                Debug.log "hi" <| viewDeal index deal

                            Nothing ->
                                Debug.log "hii" <| viewEmptyDeal
                    )

        viewEmptyDeal =
            H.div [ A.style "visibility" "hidden" ]
                [ viewDeal 0 { sell = ( Resource.coin, 10 ), buy = ( Resource.coin, 10 ), events = [] }
                ]

        viewDeal index deal =
            let
                ( sellResource, sellResourceCount ) =
                    deal.sell

                ( buyResource, buyResourceCount ) =
                    deal.buy

                sellResourceCountInInventory =
                    counts
                        |> Dict.get sellResource.name
                        |> Maybe.withDefault 0

                canCompleteDeal =
                    sellResourceCountInInventory >= sellResourceCount

                backgroundColor =
                    if canCompleteDeal then
                        "#88ff88"

                    else
                        "#ff8888"
            in
            H.button
                [ A.style "background" backgroundColor
                , A.style "border" "1px solid black"
                , A.style "padding" "1rem 1rem"
                , E.onClick
                    (if canCompleteDeal then
                        DealTaken index deal

                     else
                        Noop
                    )
                ]
                [ H.span
                    [ A.style "display" "flex"
                    , A.style "align-items" "center"
                    , A.style "font-weight" "bold"
                    , A.style "font-size" "1.1rem"
                    , A.style "padding" ".5rem"
                    , A.style "background-image" "url(/arrowred.png)"
                    , A.style "background-size" "50% 100%"
                    , A.style "background-repeat" "no-repeat"
                    , A.style "background-position" "right top"
                    ]
                    [ H.text <| String.fromInt sellResourceCount ++ "x"
                    , H.img
                        [ A.src sellResource.image
                        , A.width 24
                        , A.height 24
                        ]
                        []
                    ]
                , H.span
                    [ A.style "display" "flex"
                    , A.style "align-items" "center"
                    , A.style "justify-content" "center"
                    , A.style "font-weight" "bold"
                    , A.style "font-size" "1.1rem"
                    , A.style "padding" ".5rem"
                    , A.style "background-image" "url(/arrowgreen.png)"
                    , A.style "background-size" "50% 100%"
                    , A.style "background-position" "left top"
                    , A.style "background-repeat" "no-repeat"
                    ]
                    [ H.img
                        [ A.src buyResource.image
                        , A.width 24
                        , A.height 24
                        ]
                        []
                    , H.text <| "x" ++ String.fromInt buyResourceCount
                    ]
                ]
    in
    let
        hi =
            Debug.log "htmldealslength" (List.length htmlDeals)
    in
    H.div
        [ margin0
        , A.style "width" "100%"
        , A.style "gap" "5px"
        , A.style "display" "grid"
        , A.style "grid-template-columns" "1fr 1fr 1fr 1fr"
        ]
        htmlDeals


viewScoreBar : { r | manualScore : Int, tradingScore : Int, autoScore : Int } -> Html msg
viewScoreBar { manualScore, tradingScore, autoScore } =
    H.div [ margin0, A.style "width" "100%" ]
        [ H.p
            [ margin0
            , A.style "border" "1px solid black"
            , A.style "text-align" "center"
            , A.style "font-weight" "bold"
            ]
            [ H.text <| "Score: " ++ String.fromInt (manualScore + tradingScore + autoScore) ]
        , H.p
            [ margin0
            , A.style "font-size" ".65rem"
            , A.style "bordper" "1px solid black"
            , A.style "padding" "4px"
            , A.style "text-align" "center"
            ]
            [ H.text <| "Manual score: " ++ String.fromInt manualScore ++ " ; Trading score: " ++ String.fromInt tradingScore ++ " ; Auto score: " ++ String.fromInt autoScore ]
        ]


viewResources : { r | unlockedResources : List Resource, counts : Dict String Int, clickValues : Dict String Int } -> Html Msg
viewResources { unlockedResources, counts, clickValues } =
    let
        htmlResources : List (Html Msg)
        htmlResources =
            unlockedResources
                |> List.map
                    (\res ->
                        let
                            count = counts |> Dict.get res.name |> Maybe.withDefault 0
                            useColouredBackground =
                                clickValues
                                |> Dict.get res.name
                                |> Maybe.withDefault 0
                                |> \clickValue -> clickValue > 0
                        in
                        viewResource count useColouredBackground (ResourceClicked res) res
                    )

        numResources =
            List.length unlockedResources

        numColumns =
            min numResources 4

        gridTemplateColumns =
            List.repeat numColumns "1fr"
                |> String.join " "
    in
    H.div
        [ A.style "display" "grid"
        , A.style "grid-template-columns" gridTemplateColumns
        , A.style "width" "100%"
        , A.style "gap" "5px"
        ]
        htmlResources


viewResource : Int -> Bool -> Msg -> Resource -> Html Msg
viewResource count colouredBackground clickMsg resource =
    H.button
        [ A.style "display" "flex"
        , A.style "flex-direction" "column"
        , A.style "align-items" "center"
        , A.style "border" "1px solid black"
        , A.style "padding" "8px"
        , A.style "background"
            (if colouredBackground then
                resource.backgroundColor

             else
                "white"
            )
        , E.onClick clickMsg
        ]
        [ H.p [ margin0 ]
            [ H.text <| "Value: " ++ String.fromInt resource.value ]
        , H.img [ A.src resource.image, A.height 30, A.width 30 ] []
        , H.p [ margin0, A.style "font-weight" "bold" ]
            [ H.text (String.fromInt count) ]
        , H.p [ margin0 ]
            [ H.text resource.name ]
        ]


viewQuests : Dict String Int -> List Quest -> Html Msg
viewQuests counts quests =
    let
        viewQuest : Quest -> Html Msg
        viewQuest quest =
            let
                canCompleteQuest =
                    quest.cost
                        |> List.all
                            (\( res, resCountNeeded ) ->
                                let
                                    countInInventory =
                                        counts |> Dict.get res.name |> Maybe.withDefault 0
                                in
                                countInInventory >= resCountNeeded
                            )

                backgroundColor =
                    if canCompleteQuest then
                        "#88FF88"

                    else
                        "#FF8888"
            in
            H.button
                [ A.style "background" "white"
                , A.style "border" "none"
                , A.style "background" backgroundColor
                , A.style "padding" "16px"
                , E.onClick
                    (if canCompleteQuest then
                        QuestFulfilled quest

                     else
                        Noop
                    )
                ]
                [ H.h3
                    [ A.style "margin" "0 0 8px 0"
                    ]
                    [ H.text quest.title
                    ]
                , H.div
                    [ A.style "display" "flex"
                    , A.style "flex-direction" "row"
                    , A.style "gap" "8px"
                    ]
                    [ H.div []
                        (quest.cost
                            |> List.map
                                (\( res, resCount ) ->
                                    H.div [ A.style "display" "flex", A.style "align-items" "center" ]
                                        [ H.text <| String.fromInt resCount
                                        , H.span [ A.style "margin" "0 2px" ] [ H.text "x" ]
                                        , H.img [ A.src res.image, A.width 30, A.height 30 ] []
                                        ]
                                )
                        )
                    , H.p [ margin0 ]
                        [ H.text quest.description
                        ]
                    ]
                ]
    in
    H.div [] (List.map viewQuest quests)
