module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Attribute, Html, button, div, h1, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Time


type alias Model =
    { gameGrid : GameGrid
    , gamePhase : Phase
    , gameData : GameData
    }


type Msg
    = Tick Time.Posix
    | StartGame


type Phase
    = TitleScreen
    | Spawning
    | Falling Int
    | GameOver Int


type alias GameData =
    { speed : Int
    , score : Int
    , next : Maybe NextBlock
    }


type alias NextBlock =
    { blockSet : BlockSet
    , col : Int
    , row : Int
    }


type alias BlockSet =
    { b1 : Block
    , b2 : Block
    , b3 : Block
    }


type alias GameGrid =
    { width : Int
    , height : Int
    , columns : Array Column
    }


type alias Column =
    Array Cell


type Cell
    = Empty
    | Occupied Block


type Block
    = Red
    | Green
    | Blue


allBlocks : Array Block
allBlocks =
    Array.fromList
        [ Red
        , Green
        , Blue
        ]


initModel : () -> ( Model, Cmd Msg )
initModel =
    always <|
        noCmd
            { gameGrid = defaultGameGrid
            , gamePhase = TitleScreen
            , gameData = defaultGameData
            }


defaultGameGrid : GameGrid
defaultGameGrid =
    let
        width =
            7

        height =
            20
    in
    Empty
        |> Array.repeat height
        |> Array.repeat width
        |> GameGrid width height


defaultGameData : GameData
defaultGameData =
    { speed = 200
    , score = 0
    , next = Nothing
    }


view : Model -> Browser.Document Msg
view model =
    let
        isGameOver =
            case model.gamePhase of
                GameOver _ ->
                    True

                _ ->
                    False
    in
    { title = "Columns"
    , body =
        [ div
            [ classList [ ( "GameOverScreen", isGameOver ) ] ]
            [ heading
            , content model
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdate =
            ( model, Cmd.none )

        doUpdate fn =
            ( fn model, Cmd.none )
    in
    case msg of
        StartGame ->
            doUpdate (setGameGrid defaultGameGrid >> setPhase Spawning)

        Tick posix ->
            let
                ms =
                    Time.posixToMillis posix
            in
            case model.gamePhase of
                Spawning ->
                    if spawningBlocked model.gameGrid then
                        doUpdate (setPhase (GameOver ms))

                    else
                        doUpdate (spawnNewBlocks ms >> setPhase (Falling ms))

                Falling since ->
                    if since + model.gameData.speed <= ms then
                        doUpdate (falling ms)

                    else
                        noUpdate

                GameOver since ->
                    if since + 2000 <= ms then
                        doUpdate (setPhase TitleScreen)

                    else
                        noUpdate

                _ ->
                    noUpdate


setPhase : Phase -> Model -> Model
setPhase phase model =
    { model | gamePhase = phase }


setGameGrid : GameGrid -> Model -> Model
setGameGrid gameGrid model =
    { model | gameGrid = gameGrid }


setGameData : Model -> GameData -> Model
setGameData model gameData =
    { model | gameData = gameData }


spawningBlocked : GameGrid -> Bool
spawningBlocked gameGrid =
    (gameGrid.columns
        |> Array.get (gameGrid.width // 2 + 1)
        |> Maybe.andThen (Array.get 3)
        |> Maybe.withDefault Empty
    )
        /= Empty


spawnNewBlocks : Int -> Model -> Model
spawnNewBlocks millis model =
    model
        |> (spawnBlockSet millis |> toNextBlock model |> setNextBlock |> updateGameData)


spawnBlockSet : Int -> BlockSet
spawnBlockSet i =
    BlockSet
        (spawnBlock i)
        (spawnBlock (i // 10))
        (spawnBlock (i // 100))


spawnBlock : Int -> Block
spawnBlock i =
    Array.get
        (i |> modBy (Array.length allBlocks))
        allBlocks
        |> Maybe.withDefault Red


toNextBlock : Model -> BlockSet -> NextBlock
toNextBlock model blockSet =
    let
        col =
            model.gameGrid.width // 2
    in
    NextBlock blockSet col 0


updateGameData : (GameData -> GameData) -> Model -> Model
updateGameData fn model =
    fn model.gameData |> setGameData model


setNextBlock : NextBlock -> GameData -> GameData
setNextBlock nextBlock gameData =
    { gameData | next = Just nextBlock }



--
--spawnCellInGameGrid : Int -> Int -> Int -> GameGrid -> GameGrid
--spawnCellInGameGrid i x y grid =
--    let
--        col =
--            Array.get x grid
--                |> Maybe.withDefault Array.empty
--                |> Array.set y (spawnCell i)
--    in
--    grid
--        |> Array.set x col
--
--
--spawnCell : Int -> Cell
--spawnCell i =
--    let
--        rnd =
--            i |> modBy (Array.length allBlocks)
--
--        cell =
--            allBlocks
--                |> Array.get rnd
--                |> Maybe.withDefault Red
--    in
--    Occupied cell


falling : Int -> Model -> Model
falling ms model =
    case model.gameData.next of
        Nothing ->
            model

        Just next ->
            let
                col =
                    next.col

                row =
                    next.row

                cell =
                    if row == model.gameGrid.height then
                        Occupied Red

                    else
                        model.gameGrid.columns |> Array.get col |> Maybe.andThen (Array.get row) |> Maybe.withDefault Empty

                fall : GameData -> GameData
                fall gd =
                    { gd | next = next |> nextBlockMove Fall |> Just }
            in
            case cell of
                Empty ->
                    model |> updateGameData fall |> setPhase (Falling ms)

                _ ->
                    model |> landNextBlock |> setPhase Spawning


landNextBlock : Model -> Model
landNextBlock model =
    case model.gameData.next of
        Nothing ->
            model

        Just next ->
            let
                col =
                    next.col

                row =
                    next.row

                { b1, b2, b3 } =
                    next.blockSet

                column =
                    model.gameGrid.columns
                        |> Array.get col
                        |> Maybe.withDefault Array.empty
                        |> Array.set (row - 3) (Occupied b1)
                        |> Array.set (row - 2) (Occupied b2)
                        |> Array.set (row - 1) (Occupied b3)
            in
            model
                |> (model.gameGrid.columns
                        |> Array.set col column
                        |> GameGrid model.gameGrid.width model.gameGrid.height
                        |> setGameGrid
                   )


nextBlockMove : MoveDirection -> NextBlock -> NextBlock
nextBlockMove updateType nextBlock =
    case updateType of
        Fall ->
            { nextBlock | row = nextBlock.row + 1 }

        _ ->
            nextBlock


type MoveDirection
    = Fall
    | Rotate
    | Left
    | Right



--dropBlocks : Int -> Model -> Model
--dropBlocks ms model =
--    let
--        dropColBlocks : Column -> ( Column, Bool )
--        dropColBlocks column =
--            let
--                lowestEmptyCell =
--                    column
--                        |> Array.toIndexedList
--                        |> List.filter (\( _, cell ) -> cell == Empty)
--                        |> List.map Tuple.first
--                        |> List.maximum
--                        |> Maybe.withDefault -99
--
--                cells =
--                    column
--                        |> Array.toIndexedList
--                        |> List.filter (\( _, cell ) -> cell /= Empty)
--                        |> Dict.fromList
--
--                falling =
--                    cells
--                        |> Dict.filter
--                            (\i _ -> i < lowestEmptyCell)
--
--                newColumn =
--                    column
--                        |> Array.toIndexedList
--                        |> List.map
--                            (\( i, cell ) ->
--                                case Dict.get (i - 1) falling of
--                                    Just fallingCell ->
--                                        fallingCell
--
--                                    _ ->
--                                        case Dict.get i falling of
--                                            Just _ ->
--                                                Empty
--
--                                            _ ->
--                                                cell
--                            )
--                        |> Array.fromList
--            in
--            ( newColumn, falling |> Dict.isEmpty |> not )
--    in
--    Array.map dropColBlocks model.gameGrid
--        |> (\result ->
--                let
--                    columns =
--                        result |> Array.map Tuple.first
--
--                    didFall =
--                        result
--                            |> Array.toList
--                            |> List.map Tuple.second
--                            |> List.foldr (||) False
--                in
--                { model
--                    | gameGrid = columns
--                    , gamePhase =
--                        if didFall then
--                            Falling ms
--
--                        else
--                            Spawning
--                }
--           )


heading : Html Msg
heading =
    div
        []
        [ h1 [] [ text "COLUMNS" ] ]


content : Model -> Html Msg
content model =
    case model.gamePhase of
        TitleScreen ->
            div []
                [ button [ onClick StartGame ] [ text "START GAME" ]
                ]

        GameOver _ ->
            div []
                [ drawGameArea model.gameGrid model.gameData.next
                , div
                    [ class "GameOverPanel" ]
                    [ div [] [ text "GAME OVER" ] ]
                ]

        _ ->
            drawGameArea model.gameGrid model.gameData.next


drawGameArea : GameGrid -> Maybe NextBlock -> Html msg
drawGameArea { width, height, columns } next =
    let
        getCell_ x y =
            columns
                |> Array.get (x - 1)
                |> Maybe.andThen (Array.get (y - 1))
                |> Maybe.withDefault Empty

        getCell x y =
            case next of
                Nothing ->
                    getCell_ x y

                Just { blockSet, col, row } ->
                    if x - 1 == col && y >= row - 2 && y <= row then
                        if y + 2 == row then
                            Occupied blockSet.b1

                        else if y + 1 == row then
                            Occupied blockSet.b2

                        else
                            Occupied blockSet.b3

                    else
                        getCell_ x y
    in
    div [ class "GameArea" ]
        (List.range 1 height
            |> List.map
                (\y ->
                    div [ class "GameArea_row" ]
                        (List.range 1 width
                            |> List.map (\x -> getCell x y)
                            |> List.map getCellClass
                            |> List.map drawCell
                        )
                )
        )


getCellClass : Cell -> String
getCellClass cell =
    case cell of
        Empty ->
            "cell_empty"

        Occupied Red ->
            "cell_red"

        Occupied Green ->
            "cell_green"

        Occupied Blue ->
            "cell_blue"


drawCell : String -> Html msg
drawCell cellClass =
    div
        [ class "GameArea_cell"
        , class cellClass
        ]
        []


main : Program () Model Msg
main =
    Browser.document
        { init = initModel
        , view = view
        , update = update
        , subscriptions = always (onAnimationFrame Tick)
        }



-- HELPER FUNCTIONS


noCmd : Model -> ( Model, Cmd Msg )
noCmd =
    withCmd Cmd.none


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )
