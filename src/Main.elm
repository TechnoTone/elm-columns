module Main exposing (main)

import Browser
import Browser.Events as Browser
import GameGrid
import Html exposing (Attribute, Html, button, div, h1, h3, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import List.Extra as List
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
    | Eliminating (List GameGrid.DeadCell)
    | Collapsing


type Msg
    = Tick Time.Posix
    | StartGame
    | PlayerAction PlayerAction
    | DeadCellAnimationEnd GameGrid.Coordinate


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
        ( { gameGrid = GameGrid.init
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
            setGameGrid GameGrid.init >> setPhase (Playing 0 Controlling)
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
            if GameGrid.hasNoNext model.gameGrid then
                let
                    deadCells : List GameGrid.CoordinateTriple
                    deadCells =
                        GameGrid.checkForDeadCells model.gameGrid

                    coordinateList : List GameGrid.Coordinate
                    coordinateList =
                        deadCells |> List.concatMap (\( a, b, c ) -> [ a, b, c ])

                    totalScore =
                        List.length coordinateList

                    groupedCoordinates : List GameGrid.DeadCell
                    groupedCoordinates =
                        List.group coordinateList
                            |> List.map (\( c, l ) -> GameGrid.DeadCell c (List.length l))
                in
                if not <| List.isEmpty groupedCoordinates then
                    doUpdate (updateScore totalScore >> setPhase (Playing ms (Eliminating groupedCoordinates)))

                else if GameGrid.spawningBlocked model.gameGrid then
                    doUpdate (setPhase (GameOver ms))

                else
                    doUpdate (updateGameGrid (GameGrid.spawnNewBlocks ms) >> setPhase (Playing ms Controlling))

            else if since + model.gameData.speed <= ms then
                doUpdate (updateGameGrid GameGrid.falling >> setPhase (Playing ms Controlling))

            else
                noUpdate

        ( Tick posix, Playing since (Eliminating (cell :: rest)) ) ->
            let
                ms =
                    Time.posixToMillis posix
            in
            if since + 20 <= ms then
                doUpdate
                    (updateGameGrid (GameGrid.eliminateCell cell)
                        >> setPhase (Playing ms (Eliminating rest))
                    )

            else
                noUpdate

        ( Tick posix, Playing since (Eliminating []) ) ->
            if GameGrid.hasDeadCells model.gameGrid then
                noUpdate

            else
                doUpdate (setPhase (Playing (Time.posixToMillis posix) Controlling))

        ( Tick posix, GameOver since ) ->
            if since + 5000 <= Time.posixToMillis posix then
                doUpdate (setPhase TitleScreen)

            else
                noUpdate

        ( DeadCellAnimationEnd coordinate, _ ) ->
            doUpdate (updateGameGrid (GameGrid.removeDeadCell coordinate))

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


setScore : Int -> GameData -> GameData
setScore score gameData =
    { gameData | score = score }


updateScore : Int -> Model -> Model
updateScore score model =
    model
        |> (model.gameData
                |> setScore (model.gameData.score + score)
                |> setGameData
           )


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
                        , GameGrid.view model.gameGrid DeadCellAnimationEnd
                        , div
                            [ class "GameOverPanel" ]
                            [ div [] [ text "GAME OVER" ] ]
                        ]

                _ ->
                    div []
                        [ gameInfo model.gameData
                        , GameGrid.view model.gameGrid DeadCellAnimationEnd
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
