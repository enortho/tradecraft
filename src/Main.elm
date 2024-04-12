module Main exposing (main)

import Board as B exposing (Board)
import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = B.empty
      , currentPlayer = B.X
      , winner = Nothing
      }
    , Cmd.none
    )


type alias Model =
    { board : Board
    , currentPlayer : B.Player
    , winner : Maybe B.Player
    }


type Msg
    = TileClicked ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TileClicked ( rowI, colI ) ->
            if B.winner model.board /= Nothing then
                ( model, Cmd.none )

            else
                let
                    ( newBoard, nextPlayer, winner ) =
                        case B.tryPlace ( rowI, colI ) model.currentPlayer model.board of
                            ( Just board, winner_ ) ->
                                ( board, B.nextPlayer model.currentPlayer, winner_ )

                            ( Nothing, _ ) ->
                                ( model.board, model.currentPlayer, Nothing )
                in
                ( { model
                    | board = newBoard
                    , currentPlayer = nextPlayer
                    , winner = winner
                  }
                , Cmd.none
                )


view model =
    let
        playerToStr player =
            case player of
                B.X ->
                    "X"

                B.O ->
                    "O"

        tileToStr tile =
            case tile of
                B.Occupied player ->
                    playerToStr player

                B.Empty ->
                    " "

        board : List (List String)
        board =
            model
                |> .board
                |> B.toList
                |> List.map (List.map tileToStr)

        boardHtml =
            board
                |> List.indexedMap
                    (\rowI row ->
                        H.div [ A.class "row" ]
                            (row
                                |> List.indexedMap
                                    (\colI tile ->
                                        H.button
                                            [ A.class "tile"
                                            , E.onClick <| TileClicked ( rowI, colI )
                                            ]
                                            [ H.text tile
                                            ]
                                    )
                            )
                    )

        winnerText =
            case model.winner of
                Just player ->
                    playerToStr player ++ " wins!!!!!!!!"

                Nothing ->
                    ""
    in
    H.div []
        [ H.h1 [] [ H.text "Tic-Tac-Toe" ]
        , H.div [ A.class "board" ] boardHtml
        , H.h2 [ A.style "text-align" "center" ] [ H.text winnerText ]
        ]
