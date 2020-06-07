module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import GameGrid
import Html exposing (Attribute, Html, button, div, h1, h3, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Events.Extra.Touch as Touch exposing (Touch)
import Json.Decode as Decode
import List.Extra as List
import Task
import Time



{-
   @@   @@  @@@@  @@@@@  @@@@@@ @@
   @@@ @@@ @@  @@ @@  @@ @@     @@
   @@ @ @@ @@  @@ @@  @@ @@@@   @@
   @@   @@ @@  @@ @@  @@ @@     @@
   @@   @@  @@@@  @@@@@  @@@@@@ @@@@@@
-}


type alias Model =
    { gameGrid : GameGrid.Model
    , gamePhase : Phase
    , gameData : GameData
    , touches : List Touch
    , viewportSize : ( Float, Float )
    }


type alias GameData =
    { speed : Int
    , score : Int
    , blockCount : Int
    , eliminationCount : Int
    }


type Phase
    = TitleScreen
    | Playing Int PlayingPhase
    | GameOver Int


type PlayingPhase
    = Controlling
    | CellsDying (List GameGrid.DeadCell)
    | Collapsing


type Msg
    = GotViewport Dom.Viewport
    | WindowResize Int Int
    | Tick Time.Posix
    | StartGame
    | PlayerAction PlayerAction
    | Touch TouchEventType Touch.Event
    | DeadCellAnimationEnd GameGrid.Coordinate


type TouchEventType
    = TouchStart
    | TouchMove
    | TouchEnd


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
          , touches = []
          , viewportSize = ( 0, 0 )
          }
        , Task.perform GotViewport Dom.getViewport
        )


defaultGameData : GameData
defaultGameData =
    { speed = startSpeed
    , score = 0
    , blockCount = 0
    , eliminationCount = 0
    }


startSpeed : Int
startSpeed =
    300



{-
   @@  @@ @@@@@@ @@@@@@ @@   @@
   @@  @@   @@   @@     @@   @@
   @@  @@   @@   @@@@   @@ @ @@
    @@@@    @@   @@     @@@@@@@
     @@   @@@@@@ @@@@@@  @@ @@
-}


view : Model -> Browser.Document Msg
view model =
    let
        heading =
            h1 [] [ text "COLUMNS" ]

        viewGame =
            [ h3 [] [ text ("SCORE: " ++ String.fromInt model.gameData.score) ]
            , GameGrid.view model.gameGrid DeadCellAnimationEnd
            ]

        ( classList, content ) =
            case model.gamePhase of
                TitleScreen ->
                    ( [ class "TitleScreen" ]
                    , [ button [ onClick StartGame ] [ text "START GAME" ] ]
                    )

                GameOver _ ->
                    ( [ class "GameOverScreen" ]
                    , viewGame
                        ++ [ div
                                [ class "GameOverPanel" ]
                                [ div [] [ text "GAME OVER" ] ]
                           ]
                    )

                Playing _ _ ->
                    ( [ Touch.onStart <| Touch TouchStart
                      , Touch.onMove <| Touch TouchMove
                      , Touch.onEnd <| Touch TouchEnd
                      ]
                    , viewGame
                    )
    in
    { title = "Columns"
    , body =
        [ div
            (id "main" :: classList)
            (heading :: content)
        ]
    }



{-
   @@  @@ @@@@@  @@@@@   @@@@  @@@@@@ @@@@@@
   @@  @@ @@  @@ @@  @@ @@  @@   @@   @@
   @@  @@ @@@@@  @@  @@ @@@@@@   @@   @@@@
   @@  @@ @@     @@  @@ @@  @@   @@   @@
    @@@@  @@     @@@@@  @@  @@   @@   @@@@@@
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdate =
            ( model, Cmd.none )

        doUpdate fn =
            ( fn model, Cmd.none )

        startGame =
            setGameGrid GameGrid.init >> setGameData defaultGameData >> setPhase (Playing 0 Controlling)
    in
    case ( msg, model.gamePhase ) of
        ( GotViewport viewPort, _ ) ->
            doUpdate (setViewport viewPort)

        ( WindowResize width height, _ ) ->
            doUpdate (setWindowSize width height)

        ( StartGame, _ ) ->
            doUpdate startGame

        ( PlayerAction DropAction, TitleScreen ) ->
            doUpdate startGame

        ( PlayerAction action, Playing _ Controlling ) ->
            ( handleAction action model, Cmd.none )

        ( PlayerAction DropAction, GameOver _ ) ->
            doUpdate (setPhase TitleScreen)

        ( Touch eventType eventData, _ ) ->
            ( handleTouch eventType eventData model, Cmd.none )

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
                    doUpdate (updateGameData (incrementScore totalScore >> incrementEliminationCount) >> setPhase (Playing ms (CellsDying groupedCoordinates)))

                else if GameGrid.spawningBlocked model.gameGrid then
                    doUpdate (setPhase (GameOver ms))

                else
                    doUpdate (updateGameGrid (GameGrid.spawnNewBlocks ms (getDifficulty model.gameData)) >> updateGameData (incrementBlockCount >> adjustGameSpeed) >> setPhase (Playing ms Controlling))

            else if since + model.gameData.speed <= ms then
                doUpdate (updateGameGrid GameGrid.falling >> setPhase (Playing ms Controlling))

            else
                noUpdate

        ( Tick posix, Playing since (CellsDying (cell :: rest)) ) ->
            let
                ms =
                    Time.posixToMillis posix
            in
            if since + 20 <= ms then
                doUpdate
                    (updateGameGrid (GameGrid.eliminateCell cell)
                        >> setPhase (Playing ms (CellsDying rest))
                    )

            else
                noUpdate

        ( Tick posix, Playing since (CellsDying []) ) ->
            if GameGrid.hasDeadCells model.gameGrid then
                noUpdate

            else
                doUpdate (setPhase (Playing (Time.posixToMillis posix) Collapsing))

        ( Tick posix, Playing since Collapsing ) ->
            let
                ms =
                    Time.posixToMillis posix
            in
            if GameGrid.isCollapsible model.gameGrid then
                if since + 20 <= ms then
                    doUpdate
                        (updateGameGrid GameGrid.collapse
                            >> setPhase (Playing ms Collapsing)
                        )

                else
                    noUpdate

            else
                doUpdate (setPhase (Playing 0 Controlling))

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


handleTouch : TouchEventType -> Touch.Event -> Model -> Model
handleTouch eventType eventData model =
    case eventType of
        TouchStart ->
            model |> updateTouches (addTouches eventData.changedTouches)

        TouchMove ->
            model

        TouchEnd ->
            model
                |> convertTouches eventData.changedTouches
                |> List.foldr handleAction model
                |> updateTouches (removeTouches eventData.changedTouches)


updateTouches : (List Touch -> List Touch) -> Model -> Model
updateTouches fn model =
    { model | touches = fn model.touches }


addTouches : List Touch -> List Touch -> List Touch
addTouches newTouches existingTouches =
    List.append existingTouches newTouches


removeTouches : List Touch -> List Touch -> List Touch
removeTouches toRemove existingTouches =
    let
        ids =
            toRemove |> List.map .identifier
    in
    existingTouches |> List.filter (\t -> not <| List.member t.identifier ids)


convertTouches : List Touch -> Model -> List PlayerAction
convertTouches endedTouches { touches, viewportSize } =
    let
        matchingTouch : Touch -> Touch
        matchingTouch t =
            touches |> List.filter (.identifier >> (==) t.identifier) |> List.head |> Maybe.withDefault t

        convertTouch : Touch -> PlayerAction
        convertTouch touch =
            let
                touchX =
                    Tuple.first touch.clientPos

                clientWidth =
                    Tuple.first viewportSize

                match : Touch
                match =
                    matchingTouch touch

                distance : ( Float, Float ) -> ( Float, Float ) -> Float
                distance ( x1, y1 ) ( x2, y2 ) =
                    sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

                convertSwipe : ( Float, Float ) -> ( Float, Float ) -> PlayerAction
                convertSwipe ( x1, y1 ) ( x2, y2 ) =
                    let
                        dx =
                            x1 - x2

                        dy =
                            y1 - y2
                    in
                    if abs dx > abs dy then
                        if dx > 0 then
                            RightAction

                        else
                            LeftAction

                    else if dy > 0 then
                        RotateDownAction

                    else
                        RotateUpAction
            in
            if distance touch.clientPos match.clientPos < 8 then
                if touchX < clientWidth / 3 then
                    LeftAction

                else if touchX < clientWidth / 3 * 2 then
                    DropAction

                else
                    RightAction

            else
                convertSwipe touch.clientPos match.clientPos
    in
    List.map convertTouch endedTouches


setViewport : Dom.Viewport -> Model -> Model
setViewport viewport model =
    { model | viewportSize = ( viewport.scene.width, viewport.scene.height ) }


setWindowSize : Int -> Int -> Model -> Model
setWindowSize width height model =
    { model | viewportSize = ( toFloat width, toFloat height ) }


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


updateGameData : (GameData -> GameData) -> Model -> Model
updateGameData fn model =
    setGameData (fn model.gameData) model


incrementScore : Int -> GameData -> GameData
incrementScore increment gameData =
    { gameData | score = gameData.score + increment }


incrementBlockCount : GameData -> GameData
incrementBlockCount gameData =
    { gameData | blockCount = gameData.blockCount + 1 }


incrementEliminationCount : GameData -> GameData
incrementEliminationCount gameData =
    { gameData | eliminationCount = gameData.eliminationCount + 1 }


adjustGameSpeed : GameData -> GameData
adjustGameSpeed gameData =
    { gameData | speed = getSpeed gameData }


getSpeed : GameData -> Int
getSpeed gameData =
    startSpeed - gameData.blockCount // 2


getDifficulty : GameData -> Int
getDifficulty gameData =
    gameData.eliminationCount // 20



{-
   @@   @@   @@@@   @@@@@@  @@  @@
   @@@ @@@  @@  @@    @@    @@@ @@
   @@ @ @@  @@@@@@    @@    @@ @@@
   @@   @@  @@  @@    @@    @@  @@
   @@   @@  @@  @@  @@@@@@  @@  @@
-}


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
            , Browser.onResize WindowResize
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
