module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    { contentView : ContentView
    }


initModel : Model
initModel =
    { contentView = TitleScreen
    }


type ContentView
    = TitleScreen
    | Playing


type Msg
    = StartGame


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }


view : Model -> Html Msg
view model =
    div []
        [ heading
        , content model.contentView
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartGame ->
            { model | contentView = Playing }


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
            text "Playing"
