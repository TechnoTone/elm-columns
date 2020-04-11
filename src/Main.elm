module Main exposing (main)

import Browser
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time


type alias Model =
    { contentView : ContentView
    , gameGrid : GameGrid
    }


type alias GameGrid =
    { width : Int
    , height : Int
    , size : Int
    , cells : Dict Coordinate Cell
    }


type alias Coordinate =
    { x : Int, y : Int }


type Cell
    = Empty
    | Occupied Color


type ContentView
    = TitleScreen
    | Playing


type Msg
    = Tick Time.Posix
    | StartGame


initModel : () -> ( Model, Cmd Msg )
initModel =
    always <|
        noCmd
            { contentView = TitleScreen
            , gameGrid = initGameGrid
            }


initGameGrid : GameGrid
initGameGrid =
    { width = 7
    , height = 20
    , size = 20
    , cells = Dict.empty
    }


view : Model -> Html Msg
view model =
    div []
        [ heading
        , content model.contentView
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model | contentView = Playing }, Cmd.none )

        Tick posix ->
            ( model, Cmd.none )


heading : Html Msg
heading =
    div
        [ style "text-align" "center" ]
        [ h1 [] [ text "BOBBY'S COLUMNS" ] ]


content : ContentView -> Html Msg
content contentView =
    case contentView of
        TitleScreen ->
            div []
                [ text "Title Screen"
                , button [ onClick StartGame ] [ text "START GAME" ]
                ]

        Playing ->
            drawGameArea


drawGameArea : Html msg
drawGameArea =
    div []
        []


main : Program () Model Msg
main =
    Browser.element
        { init = initModel
        , view = view
        , update = update
        , subscriptions = always <| Time.every 1000 Tick
        }



-- HELPER FUNCTIONS


noCmd : Model -> ( Model, Cmd Msg )
noCmd =
    withCmd Cmd.none


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )
