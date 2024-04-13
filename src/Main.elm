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
      , boardStatus = B.NextPlayer B.X
      }
    , Cmd.none
    )


type alias Model =
    { board : Board
    , boardStatus : B.BoardState
    }


type Msg
    = TileClicked ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.boardStatus ) of
        ( TileClicked _, B.Draw ) ->
            ( model, Cmd.none )

        ( TileClicked _, B.Winner _ _ ) ->
            ( model, Cmd.none )

        ( TileClicked ( rowI, colI ), B.NextPlayer player ) ->
            let
                ( newBoard, newBoardStatus ) =
                    B.tryPlace ( rowI, colI ) player model.board
            in
            ( { model
                | board = newBoard
                , boardStatus = newBoardStatus
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

        directionToStr : B.WinningDirection -> String
        directionToStr dir =
            case dir of
                B.Row i ->
                    "row " ++ String.fromInt i

                B.Column i ->
                    "col " ++ String.fromInt i

                B.Diagonal i ->
                    "diagonal " ++ String.fromInt i

        statusText =
            case model.boardStatus of
                B.Draw ->
                    "Draw!"

                B.Winner player direction ->
                    "Player " ++ playerToStr player ++ " wins on " ++ directionToStr direction ++ "!!"

                B.NextPlayer _ ->
                    ""
    in
    H.div []
        [ H.h1 [] [ H.text "Tic-Tac-Toe" ]
        , H.div [ A.class "board" ] boardHtml
        , H.h2 [ A.style "text-align" "center" ] [ H.text statusText ]
        ]
