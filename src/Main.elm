module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as Browser
import Html exposing (Attribute, Html, button, div, h1, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Time


type alias Model =
    { gameGrid : GameGrid
    , gamePhase : Phase
    , gameData : GameData
    }


type Phase
    = TitleScreen
    | Playing Int
    | GameOver Int


type Msg
    = Tick Time.Posix
    | StartGame
    | KeyDown MoveDirection


type MoveDirection
    = None
    | Fall
    | FallToBottom
    | Rotate
    | Left
    | Right


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

        whenPlaying : (Int -> Model -> Model) -> ( Model, Cmd msg )
        whenPlaying fn =
            case model.gamePhase of
                Playing ms ->
                    doUpdate (fn ms)

                _ ->
                    noUpdate
    in
    case msg of
        StartGame ->
            doUpdate (setGameGrid defaultGameGrid >> setPhase (Playing 0))

        KeyDown action ->
            whenPlaying <| handleAction action

        Tick posix ->
            let
                ms =
                    Time.posixToMillis posix
            in
            case model.gamePhase of
                Playing since ->
                    if model.gameData.next == Nothing then
                        if spawningBlocked model.gameGrid then
                            doUpdate (setPhase (GameOver ms))

                        else
                            doUpdate (spawnNewBlocks ms >> setPhase (Playing ms))

                    else if since + model.gameData.speed <= ms then
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


handleAction : MoveDirection -> Int -> Model -> Model
handleAction action ms model =
    model


setPhase : Phase -> Model -> Model
setPhase phase model =
    { model | gamePhase = phase }


setGameGrid : GameGrid -> Model -> Model
setGameGrid gameGrid model =
    { model | gameGrid = gameGrid }


setGameData : GameData -> Model -> Model
setGameData gameData model =
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
    model |> (fn model.gameData |> setGameData)


setNextBlock : NextBlock -> GameData -> GameData
setNextBlock nextBlock gameData =
    { gameData | next = Just nextBlock }


clearNextBlock : GameData -> GameData
clearNextBlock gameData =
    { gameData | next = Nothing }


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
                    model |> updateGameData fall |> setPhase (Playing ms)

                _ ->
                    model |> landNextBlock


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
                |> (model.gameData |> clearNextBlock |> setGameData)
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
    div
        [ class "GameArea" ]
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
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions =
    always <|
        Sub.batch
            [ Browser.onAnimationFrame Tick
            , Browser.onKeyDown (Decode.map KeyDown keyDecoder)
            ]


keyDecoder : Decode.Decoder MoveDirection
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> MoveDirection
toDirection string =
    case String.toUpper string of
        "ARROWLEFT" ->
            Left

        "A" ->
            Left

        "ARROWRIGHT" ->
            Right

        "D" ->
            Right

        "ARROWUP" ->
            Rotate

        "W" ->
            Rotate

        "ARROWDOWN" ->
            Fall

        "S" ->
            Fall

        " " ->
            FallToBottom

        _ ->
            None



-- HELPER FUNCTIONS


noCmd : Model -> ( Model, Cmd Msg )
noCmd =
    withCmd Cmd.none


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )
