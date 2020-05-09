module Main exposing (main)

import Browser
import Browser.Events as Browser
import GameGrid
import Html exposing (Attribute, Html, button, div, h1, h3, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Time


type alias Model =
    { gameGrid : GameGrid.Model
    , gamePhase : Phase
    , gameData : GameData
    }


type alias GameData =
    { speed : Int
    , score : Int
    }


type Phase
    = TitleScreen
    | Playing Int PlayingPhase
    | GameOver Int


type PlayingPhase
    = Controlling
    | Eliminating (List GameGrid.EliminatedCell)
    | Collapsing


type Msg
    = Tick Time.Posix
    | StartGame
    | PlayerAction PlayerAction


type PlayerAction
    = None
    | DropAction
    | RotateUpAction
    | RotateDownAction
    | LeftAction
    | RightAction


initModel : () -> ( Model, Cmd Msg )
initModel =
    always <|
        ( { gameGrid = GameGrid.defaultGameGrid
          , gamePhase = TitleScreen
          , gameData = defaultGameData
          }
        , Cmd.none
        )


defaultGameData : GameData
defaultGameData =
    { speed = 200
    , score = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdate =
            ( model, Cmd.none )

        doUpdate fn =
            ( fn model, Cmd.none )

        startGame =
            setGameGrid GameGrid.defaultGameGrid >> setPhase (Playing 0 Controlling)
    in
    case ( msg, model.gamePhase ) of
        ( StartGame, _ ) ->
            doUpdate startGame

        ( PlayerAction DropAction, TitleScreen ) ->
            doUpdate startGame

        ( PlayerAction action, Playing _ Controlling ) ->
            ( handleAction action model, Cmd.none )

        ( PlayerAction DropAction, GameOver _ ) ->
            doUpdate (setPhase TitleScreen)

        ( Tick posix, Playing since Controlling ) ->
            let
                ms =
                    Time.posixToMillis posix
            in
            if model.gameGrid.next == Nothing then
                let
                    cells =
                        GameGrid.cellsToEliminate model.gameGrid
                in
                if not <| List.isEmpty cells then
                    noUpdate

                else if GameGrid.spawningBlocked model.gameGrid then
                    doUpdate (setPhase (GameOver ms))

                else
                    doUpdate (updateGameGrid (GameGrid.spawnNewBlocks ms) >> setPhase (Playing ms Controlling))

            else if since + model.gameData.speed <= ms then
                doUpdate (updateGameGrid (GameGrid.falling ms) >> setPhase (Playing ms Controlling))

            else
                noUpdate

        ( Tick posix, Playing since (Eliminating cells) ) ->
            noUpdate

        ( Tick posix, GameOver since ) ->
            if since + 5000 <= Time.posixToMillis posix then
                doUpdate (setPhase TitleScreen)

            else
                noUpdate

        _ ->
            noUpdate


handleAction : PlayerAction -> Model -> Model
handleAction action model =
    case action of
        DropAction ->
            model |> updateGameGrid GameGrid.dropToBottom |> setPhase (Playing 0 Controlling)

        RotateUpAction ->
            model |> updateGameGrid GameGrid.rotateUp

        RotateDownAction ->
            model |> updateGameGrid GameGrid.rotateDown

        LeftAction ->
            model |> updateGameGrid GameGrid.moveLeft

        RightAction ->
            model |> updateGameGrid GameGrid.moveRight

        _ ->
            model


setPhase : Phase -> Model -> Model
setPhase phase model =
    { model | gamePhase = phase }


setGameGrid : GameGrid.Model -> Model -> Model
setGameGrid gameGrid model =
    { model | gameGrid = gameGrid }


updateGameGrid : (GameGrid.Model -> GameGrid.Model) -> Model -> Model
updateGameGrid fn model =
    setGameGrid (fn model.gameGrid) model


setGameData : GameData -> Model -> Model
setGameData gameData model =
    { model | gameData = gameData }


view : Model -> Browser.Document Msg
view model =
    let
        isGameOver =
            case model.gamePhase of
                GameOver _ ->
                    True

                _ ->
                    False

        gameInfo : GameData -> Html msg
        gameInfo data =
            div
                [ class "GameInfo" ]
                [ h3 [] [ text ("SCORE: " ++ String.fromInt data.score) ] ]

        content : Html Msg
        content =
            case model.gamePhase of
                TitleScreen ->
                    div []
                        [ button [ onClick StartGame ] [ text "START GAME" ]
                        ]

                GameOver _ ->
                    div []
                        [ gameInfo model.gameData
                        , GameGrid.view model.gameGrid
                        , div
                            [ class "GameOverPanel" ]
                            [ div [] [ text "GAME OVER" ] ]
                        ]

                _ ->
                    div []
                        [ gameInfo model.gameData
                        , GameGrid.view model.gameGrid
                        ]
    in
    { title = "Columns"
    , body =
        [ div
            [ classList [ ( "GameOverScreen", isGameOver ) ] ]
            [ h1 [] [ text "COLUMNS" ]
            , content
            ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions =
    always <|
        Sub.batch
            [ Browser.onAnimationFrame Tick
            , Browser.onKeyDown (Decode.map PlayerAction keyDecoder)
            ]


keyDecoder : Decode.Decoder PlayerAction
keyDecoder =
    let
        toAction string =
            case String.toUpper string of
                "ARROWLEFT" ->
                    LeftAction

                "A" ->
                    LeftAction

                "ARROWRIGHT" ->
                    RightAction

                "D" ->
                    RightAction

                "ARROWUP" ->
                    RotateUpAction

                "W" ->
                    RotateUpAction

                "ARROWDOWN" ->
                    RotateDownAction

                "S" ->
                    RotateDownAction

                " " ->
                    DropAction

                _ ->
                    None
    in
    Decode.map toAction (Decode.field "key" Decode.string)
