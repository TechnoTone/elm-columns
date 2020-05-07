module GameGrid exposing (Block(..), Cell(..), Column, EliminatedCell, EliminationState(..), Model, NextBlock, allBlocks, cellsToEliminate, defaultGameGrid, dropToBottom, falling, getCell, getCellClass, moveLeft, moveRight, rotateDown, rotateUp, spawnNewBlocks, spawningBlocked, view)

import Array exposing (Array)
import Dict
import Html exposing (Html, div)
import Html.Attributes exposing (class)


type alias Model =
    { width : Int
    , height : Int
    , next : Maybe NextBlock
    , columns : Array Column
    }


type alias Column =
    Array Cell


type Cell
    = Empty
    | Occupied Block


type alias EliminatedCell =
    { col : Int
    , row : Int
    , state : EliminationState
    }


type EliminationState
    = Queued
    | Eliminated Int


type Block
    = Red
    | Green
    | Blue


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


allBlocks : Array Block
allBlocks =
    Array.fromList
        [ Red
        , Green
        , Blue
        ]


defaultGameGrid : Model
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
        |> Model width height Nothing


cellsToEliminate : Model -> List EliminatedCell
cellsToEliminate gameGrid =
    let
        gridCells : Dict.Dict ( Int, Int ) Cell
        gridCells =
            gameGrid.columns
                |> Array.toIndexedList
                |> List.concatMap
                    (\( x, col ) ->
                        col
                            |> Array.toIndexedList
                            |> List.map
                                (\( y, cell ) ->
                                    ( ( x, y ), cell )
                                )
                    )
                |> Dict.fromList
    in
    []


spawningBlocked : Model -> Bool
spawningBlocked model =
    (model.columns
        |> Array.get (model.width // 2)
        |> Maybe.andThen (Array.get 0)
        |> Maybe.withDefault Empty
    )
        /= Empty


spawnNewBlocks : Int -> Model -> Model
spawnNewBlocks millis model =
    model
        |> (spawnBlockSet millis |> toNextBlock model |> setNextBlock)


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
            model.width // 2
    in
    NextBlock blockSet col 0


setNextBlock : NextBlock -> Model -> Model
setNextBlock nextBlock model =
    { model | next = Just nextBlock }


updateNextBlock : (NextBlock -> NextBlock) -> Model -> Model
updateNextBlock fn model =
    case model.next of
        Nothing ->
            model

        Just next ->
            setNextBlock (fn next) model


clearNextBlock : Model -> Model
clearNextBlock model =
    { model | next = Nothing }


setColumns : Array Column -> Model -> Model
setColumns columns model =
    { model | columns = columns }


getCell : Model -> Int -> Int -> Cell
getCell { columns } x y =
    columns
        |> Array.get x
        |> Maybe.andThen (Array.get y)
        |> Maybe.withDefault Empty


view : Model -> Html msg
view ({ width, height, next } as gameGrid) =
    let
        drawCell cellClass =
            div
                [ class "GameArea_cell"
                , class cellClass
                ]
                []

        getCell_ x y =
            case next of
                Nothing ->
                    getCell gameGrid x y

                Just { blockSet, col, row } ->
                    if x == col && y >= row - 2 && y <= row then
                        if y + 2 == row then
                            Occupied blockSet.b1

                        else if y + 1 == row then
                            Occupied blockSet.b2

                        else
                            Occupied blockSet.b3

                    else
                        getCell gameGrid x y
    in
    div
        [ class "GameArea" ]
        (List.range 0 (height - 1)
            |> List.map
                (\y ->
                    div [ class "GameArea_row" ]
                        (List.range 0 (width - 1)
                            |> List.map (\x -> getCell_ x y)
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


falling : Int -> Model -> Model
falling ms model =
    case model.next of
        Nothing ->
            model

        Just next ->
            let
                col =
                    next.col

                row =
                    next.row

                cellBelow =
                    if row == model.height - 1 then
                        Occupied Red

                    else
                        model.columns |> Array.get col |> Maybe.andThen (Array.get (row + 1)) |> Maybe.withDefault Empty
            in
            case cellBelow of
                Empty ->
                    fallOneRow model

                _ ->
                    model |> landNextBlock next



--
--nextBlockUpdate : NextBlockUpdates -> NextBlock -> Model -> NextBlock
--nextBlockUpdate updateType nextBlock model =
--    let
--        cellIsEmpty cell =
--            cell == Empty
--
--        colCells : Int -> Array Cell
--        colCells col =
--            model.columns |> Array.get col |> Maybe.withDefault Array.empty
--
--        bottomEmptyCell : Int -> Int
--        bottomEmptyCell col =
--            colCells col
--                |> Array.toIndexedList
--                |> List.filter (Tuple.second >> cellIsEmpty)
--                |> List.map Tuple.first
--                |> List.reverse
--                |> List.head
--                |> Maybe.withDefault 0
--
--        canMoveTo col =
--            (col >= 0)
--                && (col <= (model.width - 1))
--                && ((colCells col |> Array.get nextBlock.row |> Maybe.withDefault Empty) == Empty)
--
--        rotateUp : BlockSet -> BlockSet
--        rotateUp blockSet =
--            BlockSet blockSet.b2 blockSet.b3 blockSet.b1
--
--        rotateDown : BlockSet -> BlockSet
--        rotateDown blockSet =
--            BlockSet blockSet.b3 blockSet.b1 blockSet.b2
--    in
--    case updateType of
--        DropToBottom ->
--            { nextBlock | row = bottomEmptyCell nextBlock.col }
--
--        RotateUp ->
--            { nextBlock | blockSet = rotateUp nextBlock.blockSet }
--
--        RotateDown ->
--            { nextBlock | blockSet = rotateDown nextBlock.blockSet }
--
--        MoveLeft ->
--            if canMoveTo (nextBlock.col - 1) then
--                { nextBlock | col = nextBlock.col - 1 }
--
--            else
--                nextBlock
--
--        MoveRight ->
--            if canMoveTo (nextBlock.col + 1) then
--                { nextBlock | col = nextBlock.col + 1 }
--
--            else
--                nextBlock


updateHelper : (NextBlock -> Model) -> Model -> Model
updateHelper fn model =
    case model.next of
        Nothing ->
            model

        Just next ->
            fn next


fallOneRow : Model -> Model
fallOneRow model =
    updateNextBlock
        (\nb -> { nb | row = nb.row + 1 })
        model


cellIsEmpty : Cell -> Bool
cellIsEmpty cell =
    cell == Empty


cellIsNotEmpty : Cell -> Bool
cellIsNotEmpty cell =
    cell /= Empty


colCells : Model -> Int -> Array Cell
colCells model col =
    model.columns |> Array.get col |> Maybe.withDefault Array.empty


bottomEmptyCell : Model -> Int -> Int
bottomEmptyCell model col =
    colCells model col
        |> Array.toIndexedList
        |> List.filter (Tuple.second >> cellIsEmpty)
        |> List.map Tuple.first
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0


dropToBottom : Model -> Model
dropToBottom model =
    updateNextBlock
        (\nb -> { nb | row = bottomEmptyCell model nb.col })
        model


rotateUp : Model -> Model
rotateUp model =
    let
        update : BlockSet -> BlockSet
        update blockSet =
            BlockSet blockSet.b2 blockSet.b3 blockSet.b1
    in
    updateNextBlock
        (\nb -> { nb | blockSet = update nb.blockSet })
        model


rotateDown : Model -> Model
rotateDown model =
    let
        update : BlockSet -> BlockSet
        update blockSet =
            BlockSet blockSet.b3 blockSet.b1 blockSet.b2
    in
    updateNextBlock
        (\nb -> { nb | blockSet = update nb.blockSet })
        model


canMoveTo : Model -> Int -> Bool
canMoveTo model col =
    case model.next of
        Just next ->
            (col >= 0)
                && (col <= (model.width - 1))
                && cellIsEmpty (getCell model col next.row)

        Nothing ->
            False


moveNextBlock : Model -> Int -> NextBlock -> NextBlock
moveNextBlock model col nb =
    if canMoveTo model col then
        { nb | col = col }

    else
        nb


moveLeft : Model -> Model
moveLeft model =
    updateNextBlock
        (\nb -> moveNextBlock model (nb.col - 1) nb)
        model


moveRight : Model -> Model
moveRight model =
    updateNextBlock
        (\nb -> moveNextBlock model (nb.col + 1) nb)
        model


landNextBlock : NextBlock -> Model -> Model
landNextBlock next model =
    let
        col =
            next.col

        row =
            next.row

        { b1, b2, b3 } =
            next.blockSet

        column =
            model.columns
                |> Array.get col
                |> Maybe.withDefault Array.empty
                |> Array.set (row - 2) (Occupied b1)
                |> Array.set (row - 1) (Occupied b2)
                |> Array.set (row - 0) (Occupied b3)
    in
    model
        |> clearNextBlock
        |> setColumns (Array.set col column model.columns)
