module GameGrid exposing
    ( Block(..)
    , Cell(..)
    , Column
    , Coordinate
    , CoordinateTriple
    , DeadCell
    , Model
    , NextBlock
    , allBlocks
    , checkForDeadCells
    , collapse
    , dropToBottom
    , eliminateCell
    , falling
    , hasDeadCells
    , hasNext
    , hasNoNext
    , init
    , isCollapsible
    , moveLeft
    , moveRight
    , removeDeadCell
    , rotateDown
    , rotateUp
    , spawnNewBlocks
    , spawningBlocked
    , view
    )

import Array exposing (Array)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Json.Decode as Decode


type alias Model =
    { width : Int
    , height : Int
    , next : Maybe NextBlock
    , columns : Array Column
    }


type alias NextBlock =
    { blockSet : BlockTriple
    , coordinate : Coordinate
    }


type alias BlockTriple =
    { b1 : Block
    , b2 : Block
    , b3 : Block
    }


type alias Column =
    Array Cell


type Cell
    = Empty
    | AliveBlock Block
    | DeadBlock Int Block


type Block
    = Red
    | Green
    | Blue
    | Cyan
    | Yellow
    | Magenta
    | Orange


type alias DeadCell =
    { coordinate : Coordinate
    , multiplier : Int
    }


type alias Coordinate =
    { col : Int
    , row : Int
    }


type alias CoordinateTriple =
    ( Coordinate, Coordinate, Coordinate )


allBlocks : Array Block
allBlocks =
    Array.fromList
        [ Red
        , Green
        , Blue
        , Cyan
        , Yellow
        , Magenta
        , Orange
        ]


init : Model
init =
    let
        width =
            7

        height =
            20

        next =
            Nothing
    in
    Empty
        |> Array.repeat height
        |> Array.repeat width
        |> Model width height next


view : Model -> (Coordinate -> msg) -> Html msg
view ({ width, height, next } as gameGrid) deadCellAnimationEndMsg =
    let
        drawCell coordinate cell =
            div
                [ class "GameArea_cell" ]
                [ div
                    (List.append
                        [ class <| cellClass cell ]
                        (deadCellAnimationHook cell coordinate)
                    )
                    []
                ]

        captureAnimEnd : Coordinate -> List (Html.Attribute msg)
        captureAnimEnd coordinate =
            [ "webkitAnimationEnd", "oanimationend", "msAnimationEnd", "animationend" ]
                |> List.map
                    (\event ->
                        on event (Decode.succeed (deadCellAnimationEndMsg coordinate))
                    )

        deadCellAnimationHook : Cell -> Coordinate -> List (Html.Attribute msg)
        deadCellAnimationHook cell coordinate =
            case cell of
                DeadBlock _ _ ->
                    captureAnimEnd coordinate

                _ ->
                    []

        cellClass cell =
            case cell of
                Empty ->
                    "cell_empty"

                AliveBlock block ->
                    "cell_" ++ blockClass block

                DeadBlock multiplier block ->
                    "cell_dead cell_" ++ blockClass block

        blockClass block =
            case block of
                Red ->
                    "red"

                Green ->
                    "green"

                Blue ->
                    "blue"

                Cyan ->
                    "cyan"

                Yellow ->
                    "yellow"

                Magenta ->
                    "magenta"

                Orange ->
                    "orange"

        getCell_ x y =
            case next of
                Nothing ->
                    getCell gameGrid (Coordinate x y)

                Just { blockSet, coordinate } ->
                    if x == coordinate.col && y >= coordinate.row - 2 && y <= coordinate.row then
                        if y + 2 == coordinate.row then
                            AliveBlock blockSet.b1

                        else if y + 1 == coordinate.row then
                            AliveBlock blockSet.b2

                        else
                            AliveBlock blockSet.b3

                    else
                        getCell gameGrid (Coordinate x y)
    in
    div
        [ class "GameArea" ]
        (List.range 0 (height - 1)
            |> List.map
                (\y ->
                    div [ class "GameArea_row" ]
                        (List.range 0 (width - 1)
                            |> List.map (\x -> drawCell (Coordinate x y) (getCell_ x y))
                        )
                )
        )



-- Model update functions


spawnNewBlocks : Int -> Model -> Model
spawnNewBlocks millis model =
    let
        spawnBlockSet : Int -> BlockTriple
        spawnBlockSet i =
            BlockTriple
                (spawnBlock i)
                (spawnBlock (i // 10))
                (spawnBlock (i // 100))

        spawnBlock : Int -> Block
        spawnBlock i =
            Array.get
                (i |> modBy (Array.length allBlocks))
                allBlocks
                |> Maybe.withDefault Red

        toNextBlock : BlockTriple -> NextBlock
        toNextBlock blockSet =
            let
                col =
                    model.width // 2
            in
            NextBlock blockSet (Coordinate col 0)
    in
    model
        |> (spawnBlockSet millis |> toNextBlock |> setNextBlock)


falling : Model -> Model
falling model =
    case model.next of
        Nothing ->
            model

        Just next ->
            let
                { col, row } =
                    next.coordinate

                cellBelow =
                    if row == model.height - 1 then
                        AliveBlock Red

                    else
                        model.columns |> Array.get col |> Maybe.andThen (Array.get (row + 1)) |> Maybe.withDefault Empty
            in
            case cellBelow of
                Empty ->
                    updateNextBlock (updateCoordinate (updateCoordinateRow ((+) 1))) model

                _ ->
                    landNextBlock next model


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


updateCoordinate : (Coordinate -> Coordinate) -> NextBlock -> NextBlock
updateCoordinate fn next =
    { next | coordinate = fn next.coordinate }


updateCoordinateRow : (Int -> Int) -> Coordinate -> Coordinate
updateCoordinateRow fn coordinate =
    { coordinate | row = fn coordinate.row }


updateCoordinateCol : (Int -> Int) -> Coordinate -> Coordinate
updateCoordinateCol fn coordinate =
    { coordinate | col = fn coordinate.col }


clearNextBlock : Model -> Model
clearNextBlock model =
    { model | next = Nothing }


eliminateCell : DeadCell -> Model -> Model
eliminateCell { coordinate, multiplier } model =
    model |> updateCell coordinate (toEliminatedBlock (getCell model coordinate) multiplier)


removeDeadCell : Coordinate -> Model -> Model
removeDeadCell coordinate model =
    model |> updateCell coordinate Empty


collapse : Model -> Model
collapse model =
    let
        collapseColumn : Column -> Column
        collapseColumn column =
            let
                l =
                    column |> Array.toList

                n =
                    column |> bottomEmptyCell
            in
            if n > 0 then
                Empty
                    :: List.take n l
                    ++ List.drop (n + 1) l
                    |> Array.fromList

            else
                column
    in
    model |> setColumns (model.columns |> Array.map collapseColumn)


toEliminatedBlock : Cell -> Int -> Cell
toEliminatedBlock aliveCell multiplier =
    case aliveCell of
        Empty ->
            Empty

        AliveBlock block ->
            DeadBlock multiplier block

        DeadBlock _ _ ->
            aliveCell


updateCell : Coordinate -> Cell -> Model -> Model
updateCell { col, row } cell model =
    model
        |> (Array.set col
                (Array.get col model.columns
                    |> Maybe.withDefault Array.empty
                    |> Array.set row cell
                )
                model.columns
                |> setColumns
           )


setColumns : Array Column -> Model -> Model
setColumns columns model =
    { model | columns = columns }


dropToBottom : Model -> Model
dropToBottom model =
    let
        colCells : Int -> Array Cell
        colCells col =
            model.columns |> Array.get col |> Maybe.withDefault Array.empty

        newCoordinate : Coordinate -> Coordinate
        newCoordinate coordinate =
            { coordinate | row = bottomEmptyCell <| colCells coordinate.col }
    in
    updateNextBlock (\nb -> { nb | coordinate = newCoordinate nb.coordinate }) model


rotateUp : Model -> Model
rotateUp model =
    let
        update : BlockTriple -> BlockTriple
        update blockSet =
            BlockTriple blockSet.b2 blockSet.b3 blockSet.b1
    in
    updateNextBlock
        (\nb -> { nb | blockSet = update nb.blockSet })
        model


rotateDown : Model -> Model
rotateDown model =
    let
        update : BlockTriple -> BlockTriple
        update blockSet =
            BlockTriple blockSet.b3 blockSet.b1 blockSet.b2
    in
    updateNextBlock
        (\nb -> { nb | blockSet = update nb.blockSet })
        model


moveNextBlock : Model -> Int -> NextBlock -> NextBlock
moveNextBlock model col nb =
    case model.next of
        Just next ->
            let
                newCoordinate =
                    Coordinate col next.coordinate.row
            in
            if
                (col >= 0)
                    && (col <= (model.width - 1))
                    && cellIsEmpty (getCell model newCoordinate)
            then
                { nb | coordinate = newCoordinate }

            else
                nb

        Nothing ->
            nb


moveLeft : Model -> Model
moveLeft model =
    updateNextBlock
        (\nb -> moveNextBlock model (nb.coordinate.col - 1) nb)
        model


moveRight : Model -> Model
moveRight model =
    updateNextBlock
        (\nb -> moveNextBlock model (nb.coordinate.col + 1) nb)
        model


landNextBlock : NextBlock -> Model -> Model
landNextBlock next model =
    let
        { col, row } =
            next.coordinate

        { b1, b2, b3 } =
            next.blockSet

        column =
            model.columns
                |> Array.get col
                |> Maybe.withDefault Array.empty
                |> Array.set (row - 2) (AliveBlock b1)
                |> Array.set (row - 1) (AliveBlock b2)
                |> Array.set (row - 0) (AliveBlock b3)
    in
    model
        |> clearNextBlock
        |> setColumns (Array.set col column model.columns)



-- status functions


getCell : Model -> Coordinate -> Cell
getCell { columns } { col, row } =
    columns
        |> Array.get col
        |> Maybe.andThen (Array.get row)
        |> Maybe.withDefault Empty


cellIsEmpty : Cell -> Bool
cellIsEmpty cell =
    cell == Empty


cellIsAlive : Cell -> Bool
cellIsAlive cell =
    case cell of
        AliveBlock _ ->
            True

        _ ->
            False


cellIsDead : Cell -> Bool
cellIsDead cell =
    case cell of
        DeadBlock _ _ ->
            True

        _ ->
            False


hasNext : Model -> Bool
hasNext { next } =
    next /= Nothing


hasNoNext : Model -> Bool
hasNoNext =
    not << hasNext


spawningBlocked : Model -> Bool
spawningBlocked model =
    (model.columns
        |> Array.get (model.width // 2)
        |> Maybe.andThen (Array.get 0)
        |> Maybe.withDefault Empty
    )
        /= Empty


checkForDeadCells : Model -> List CoordinateTriple
checkForDeadCells model =
    let
        cellNotEmpty ( _, cell ) =
            cell /= Empty

        cellsMatch : Cell -> Cell -> Cell -> Bool
        cellsMatch a b c =
            a == b && a == c

        getCellGroups : ( ( Int, Int ), Cell ) -> Maybe CoordinateTriple
        getCellGroups ( ( x, y ), cell ) =
            let
                gc : Int -> Int -> Cell
                gc dx dy =
                    getCell model (Coordinate (x + dx) (y + dy))

                ec : Int -> Int -> Coordinate
                ec dx dy =
                    Coordinate (x + dx) (y + dy)
            in
            if cellsMatch (gc -1 0) cell (gc 1 0) then
                Just <| ( ec -1 0, ec 0 0, ec 1 0 )

            else if cellsMatch (gc 0 -1) cell (gc 0 1) then
                Just ( ec 0 -1, ec 0 0, ec 0 1 )

            else if cellsMatch (gc -1 -1) cell (gc 1 1) then
                Just ( ec -1 -1, ec 0 0, ec 1 1 )

            else if cellsMatch (gc 1 -1) cell (gc -1 1) then
                Just ( ec 1 -1, ec 0 0, ec -1 1 )

            else
                Nothing
    in
    model.columns
        |> Array.toIndexedList
        |> List.concatMap
            (\( x, col ) ->
                col
                    |> Array.toIndexedList
                    |> List.filter cellNotEmpty
                    |> List.map
                        (\( y, cell ) ->
                            ( ( x, y ), cell )
                        )
            )
        |> List.filterMap getCellGroups


hasDeadCells : Model -> Bool
hasDeadCells model =
    model.columns
        |> Array.toList
        |> List.concatMap Array.toList
        |> List.any cellIsDead


bottomEmptyCell : Array Cell -> Int
bottomEmptyCell cells =
    cells
        |> Array.toIndexedList
        |> List.filter (Tuple.second >> cellIsEmpty)
        |> List.map Tuple.first
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0


isCollapsible : Model -> Bool
isCollapsible model =
    let
        topAliveCell : Array Cell -> Int
        topAliveCell cells =
            cells
                |> Array.toIndexedList
                |> List.filter (Tuple.second >> cellIsAlive)
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault 99
    in
    model.columns
        |> Array.map (\cells -> bottomEmptyCell cells > topAliveCell cells)
        |> Array.toList
        |> List.foldr (||) False
