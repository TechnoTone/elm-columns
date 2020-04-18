module Main exposing (main)

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
    | Falling
    | GameOver


type alias GameGrid =
    { width : Int
    , height : Int
    , cells : CellDict
    }


type alias CellDict =
    Dict Coordinate Cell


type alias Coordinate =
    String


getCoordinate : Int -> Int -> Coordinate
getCoordinate x y =
    String.fromInt x ++ "," ++ String.fromInt y


fromCoordinate : Coordinate -> ( Int, Int )
fromCoordinate c =
    let
        parsed =
            String.split "," c
                |> List.filterMap String.toInt
    in
    case parsed of
        [ x, y ] ->
            ( x, y )

        _ ->
            ( 0, 0 )


type Cell
    = Empty
    | Occupied Block


type Block
    = Red
    | Green
    | Blue


getBlockList : List Block
getBlockList =
    [ Red, Green, Blue ]


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
    { width = 7
    , height = 20
    , cells = Dict.empty
    }



--Dict.fromList
--    [ ( getCoordinate 4 1, Occupied Red )
--    , ( getCoordinate 4 2, Occupied Green )
--    , ( getCoordinate 4 3, Occupied Blue )
--    ]


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
            ( model, Cmd.none )


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
    div [ class "GameArea" ]
        (List.range 1 gameGrid.height
            |> List.map
                (\y ->
                    div [ class "GameArea_row" ]
                        (List.range 1 gameGrid.width
                            |> List.map (\x -> getCoordinate x y)
                            |> List.map (drawCell gameGrid.cells)
                        )
                )
        )


drawCell : Dict Coordinate Cell -> Coordinate -> Html msg
drawCell dict coord =
    div
        [ class "GameArea_cell"
        , class
            (Dict.get coord dict
                |> Maybe.withDefault Empty
                |> getCellClass
            )
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
