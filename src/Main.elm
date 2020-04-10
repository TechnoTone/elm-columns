module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)


type alias Model =
    {}


type Msg
    = DontKnowWhatYet


main : Program () Model Msg
main =
    Browser.sandbox
        { init = {}
        , view = view
        , update = update
        }


view : Model -> Html Msg
view model =
    div []
        [ heading
        , content
        ]


update : Msg -> Model -> Model
update msg model =
    model


heading =
    div
        [ style "text-align" "center" ]
        [ h1 [] [ text "BOBBY'S COLUMNS" ] ]


content =
    text "game"
