module Main exposing (main)

import Browser
import Deal exposing (Deal)
import Dict exposing (Dict)
import Event
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Platform.Cmd as Cmd
import Resource exposing (Resource)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { clickableResources : List Resource
    , viewableResources : List Resource
    , lockedResources : List Resource
    , manualScore : Int
    , tradingScore : Int
    , autoScore : Int
    , triggers : List Event.ResourceTrigger
    , counts : Dict String Int
    , deals : Maybe (List Deal)
    }


type Msg
    = Noop
    | ResourceClicked Resource
    | ResourceTriggersSatisfied Event.Event (List Event.Event)
    | DealClicked Deal


type alias ResourceGroup =
    { clickable : List Resource
    , viewable : List Resource
    , locked : List Resource
    }


initialModel =
    { clickableResources = [ Resource.coin ]
    , viewableResources = []
    , lockedResources = [ Resource.wood, Resource.bricks ]
    , manualScore = 0
    , tradingScore = 0
    , autoScore = 0
    , counts = Dict.empty
    , triggers = [ startDealsTrigger ]
    , deals = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { clickableResources = [ Resource.coin ]
      , viewableResources = []
      , lockedResources = [ Resource.wood, Resource.bricks ]
      , manualScore = 0
      , tradingScore = 0
      , autoScore = 0
      , counts = Dict.empty
      , triggers = [ startDealsTrigger ]
      , deals = Just [ { sell = ( Resource.coin, 14 ), buy = ( Resource.wood, 20 ) } ]
      }
    , Cmd.none
    )


startDealsTrigger : Event.ResourceTrigger
startDealsTrigger =
    { resourcesNeeded = [ ( Resource.coin, 30 ) ]
    , event = Event.StartDeals
    }


checkResourceTriggers : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkResourceTriggers ( model, cmdmsg ) =
    let
        ( eventsToSend, unsatisfiedTriggers ) =
            model.triggers
                |> Event.checkTriggers model.counts
    in
    case eventsToSend of
        [] ->
            ( model, cmdmsg )

        firstEvent :: restEvents ->
            let
                ( newModel, newCmdMsg ) =
                    update (ResourceTriggersSatisfied firstEvent restEvents) model
            in
            ( { newModel | triggers = unsatisfiedTriggers }, Cmd.batch [ cmdmsg, newCmdMsg ] )


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
            in
            ( { model
                | counts = model.counts |> Dict.insert res.name (currentCount + 1)
                , manualScore = model.manualScore + res.value
              }
            , Cmd.none
            )
                |> checkResourceTriggers

        DealClicked deal ->
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
            if sellResourceCountInInventory < sellResourceCount then
                ( model, Cmd.none )

            else
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
                    , tradingScore = model.tradingScore + Deal.value deal
                  }
                , Cmd.none
                )
                    |> checkResourceTriggers

        ResourceTriggersSatisfied firstEvent restEvents ->
            let
                handleEvent : Event.Event -> Model -> Model
                handleEvent event model_ =
                    case event of
                        Event.StartDeals ->
                            { model_
                                | deals =
                                    case model_.deals of
                                        Just deals ->
                                            Just deals

                                        Nothing ->
                                            Just []
                            }

                        Event.UnlockResource res ->
                            model_

                        Event.StartQuest quest ->
                            model_

                newModel =
                    List.foldl handleEvent model (firstEvent :: restEvents)
            in
            ( newModel
            , Cmd.none
            )


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
            (case model.deals of
                Just deals ->
                    [ viewDeals model.counts deals, viewResources model, viewScoreBar model ]

                Nothing ->
                    [ viewResources model, viewScoreBar model ]
            )
        , H.p
            []
            [ H.text "" ]
        ]


viewDeals : Dict String Int -> List Deal -> Html Msg
viewDeals counts deals =
    H.div
        [ margin0
        , A.style "width" "100%"
        , A.style "gap" "5px"
        , A.style "display" "grid"
        , A.style "grid-template-columns" "1fr 1fr 1fr 1fr"
        ]
        (deals
            |> List.map
                (\deal ->
                    let
                        ( sellResource, sellResourceCount ) =
                            deal.sell

                        ( buyResource, buyResourceCount ) =
                            deal.buy

                        sellResourceCountInInventory =
                            counts
                                |> Dict.get sellResource.name
                                |> Maybe.withDefault 0

                        backgroundColor =
                            if sellResourceCountInInventory < sellResourceCount then
                                "#ff8888"

                            else
                                "#88ff88"
                    in
                    H.button
                        [ A.style "background" backgroundColor
                        , A.style "border" "1px solid black"
                        , A.style "padding" "1rem 1rem"
                        , E.onClick (DealClicked deal)
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
                )
        )


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


viewResources : { r | clickableResources : List Resource, viewableResources : List Resource, counts : Dict String Int } -> Html Msg
viewResources model =
    let
        clickableResources : List (Html Msg)
        clickableResources =
            model.clickableResources
                |> List.map
                    (\res ->
                        let
                            count =
                                model.counts |> Dict.get res.name |> Maybe.withDefault 0
                        in
                        viewResource count True (ResourceClicked res) res
                    )

        viewableResources : List (Html Msg)
        viewableResources =
            model.viewableResources
                |> List.map
                    (\res ->
                        let
                            count =
                                model.counts |> Dict.get res.name |> Maybe.withDefault 0
                        in
                        viewResource count False Noop res
                    )

        numResources =
            List.length clickableResources + List.length viewableResources

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
        (List.append clickableResources viewableResources)


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
