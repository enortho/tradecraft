module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Resource exposing (Resource)


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
    , score : Int
    , counts : Dict String Int
    }


type Msg
    = Noop
    | ResourceClicked Resource


init : () -> ( Model, Cmd Msg )
init _ =
    ( { clickableResources = [ Resource.coin ]
      , viewableResources = [ Resource.wood, Resource.bricks ]
      , lockedResources = [ Resource.wood ]
      , score = 0
      , counts = Dict.empty
      }
    , Cmd.none
    )


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
              }
            , Cmd.none
            )


margin0 =
    A.style "margin" "0"


view : Model -> Html Msg
view model =
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
    in
    H.div []
        [ H.div
            [ A.style "display" "flex"
            , A.style "gap" "5px"
            ]
            (List.append clickableResources viewableResources)
        , H.p [ margin0 ] [ H.text <| "Score: " ++ String.fromInt model.score ]
        ]


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
