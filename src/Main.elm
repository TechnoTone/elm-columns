module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, h1, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Time


type alias Model =
    { gameGrid : GameGrid
    , gamePhase : Phase
    }


type Phase
    = TitleScreen
    | Spawning
    | Falling Int
    | GameOver


type alias GameGrid =
    Array Column


type alias Column =
    Array Cell


type Cell
    = Empty
    | Occupied Block


type Block
    = Red
    | Green
    | Blue


getBlockList : List Block
getBlockList =
    [ Red
    , Green
    , Blue
    ]


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


type Msg
    = Tick Time.Posix
    | StartGame


initModel : () -> ( Model, Cmd Msg )
initModel =
    always <|
        noCmd
            { gameGrid = initGameGrid
            , gamePhase = TitleScreen
            }


initGameGrid : GameGrid
initGameGrid =
    let
        width =
            7

        height =
            20
    in
    Empty
        |> Array.repeat (height + 3)
        |> Array.repeat width


view : Model -> Browser.Document Msg
view model =
    { title = "Columns"
    , body =
        [ div []
            [ stylesheet
            , heading
            , content model
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model
                | gameGrid = initGameGrid
                , gamePhase = Spawning
              }
            , Cmd.none
            )

        Tick posix ->
            let
                ms =
                    Time.posixToMillis posix
            in
            case model.gamePhase of
                Spawning ->
                    ( model
                        |> spawnNewBlocks ms
                        |> setPhase (Falling ms)
                    , Cmd.none
                    )

                Falling since ->
                    if since + 100 <= ms then
                        ( model
                            |> dropBlocks ms
                            |> setPhase (Falling ms)
                        , Cmd.none
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )


setPhase : Phase -> Model -> Model
setPhase phase model =
    { model | gamePhase = phase }


spawnNewBlocks : Int -> Model -> Model
spawnNewBlocks millis model =
    let
        millis2 =
            round <| (toFloat millis / 10)

        millis3 =
            round <| (toFloat millis / 100)
    in
    { model
        | gameGrid =
            model.gameGrid
                |> spawnCellInGameGrid millis 3 0
                |> spawnCellInGameGrid millis2 3 1
                |> spawnCellInGameGrid millis3 3 2
    }


spawnCellInGameGrid : Int -> Int -> Int -> GameGrid -> GameGrid
spawnCellInGameGrid i x y grid =
    let
        col =
            Array.get x grid
                |> Maybe.withDefault Array.empty
                |> Array.set y (spawnCell i)
    in
    grid
        |> Array.set x col


spawnCell : Int -> Cell
spawnCell i =
    let
        rnd =
            i |> modBy (List.length getBlockList)

        cell =
            getBlockList
                |> Array.fromList
                |> Array.get rnd
                |> Maybe.withDefault Red
    in
    Occupied cell


dropBlocks : Int -> Model -> Model
dropBlocks ms model =
    let
        dropColBlocks : Column -> ( Column, Bool )
        dropColBlocks column =
            let
                lowestEmptyCell =
                    column
                        |> Array.toIndexedList
                        |> List.filter (\( _, cell ) -> cell == Empty)
                        |> List.reverse
                        |> List.head
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault -99

                cells =
                    column
                        |> Array.toIndexedList
                        |> List.filter (\( _, cell ) -> cell /= Empty)
                        |> Dict.fromList

                falling =
                    cells
                        |> Dict.filter
                            (\i _ -> i < lowestEmptyCell)

                newColumn =
                    column
                        |> Array.toIndexedList
                        |> List.map
                            (\( i, cell ) ->
                                case Dict.get (i - 1) falling of
                                    Just fallingCell ->
                                        fallingCell

                                    _ ->
                                        case Dict.get i falling of
                                            Just _ ->
                                                Empty

                                            _ ->
                                                cell
                            )
                        |> Array.fromList
            in
            ( newColumn, falling |> Dict.isEmpty |> not )
    in
    model.gameGrid
        |> Array.map dropColBlocks
        |> (\result ->
                let
                    columns =
                        result |> Array.map Tuple.first

                    didFall =
                        result
                            |> Array.toList
                            |> List.map Tuple.second
                            |> List.foldr (||) False
                in
                { model
                    | gameGrid = columns
                    , gamePhase =
                        if didFall then
                            Falling ms

                        else
                            Spawning
                }
           )


heading : Html Msg
heading =
    div
        [ style "text-align" "center" ]
        [ h1 [] [ text "BOBBY'S COLUMNS" ] ]


content : Model -> Html Msg
content model =
    case model.gamePhase of
        TitleScreen ->
            div []
                [ text "Title Screen"
                , button [ onClick StartGame ] [ text "START GAME" ]
                ]

        _ ->
            drawGameArea model.gameGrid


drawGameArea : GameGrid -> Html msg
drawGameArea gameGrid =
    let
        width =
            gameGrid |> Array.length

        height =
            gameGrid |> Array.get 1 |> Maybe.map Array.length |> Maybe.withDefault 0

        getCell x y =
            gameGrid
                |> Array.get (x - 1)
                |> Maybe.andThen (Array.get (y + 2))
                |> Maybe.withDefault Empty
    in
    div [ class "GameArea" ]
        (List.range 1 (height - 3)
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


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ text """
@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');

body { font-family: "Roboto"; }

.GameArea {
    width: fit-content;
    height: fit-content;
    border: 2px solid red;
}

.GameArea_row {
    display: flex;
    width: fit-content;
    height: fit-content;
}

.GameArea_cell {
    width: 20px;
    height: 20px;
    background: black;
    border: 0;
    color: white;
}

.GameArea_cell.cell_red {
    background: red;
}

.GameArea_cell.cell_green {
    background: green;
}

.GameArea_cell.cell_blue {
    background: blue;
}
"""
        ]



-- HELPER FUNCTIONS


noCmd : Model -> ( Model, Cmd Msg )
noCmd =
    withCmd Cmd.none


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )
