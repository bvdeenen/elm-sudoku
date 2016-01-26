module Sudoku where

import Dict

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Set 
import String
import Maybe
import Array
import Utils exposing (..)
import Debug



-- MODEL

type Cell = Filled Int | Possibles (Set.Set Int) | Bug
type alias SudokuModel = Matrix (Cell)
type alias Model = {
    old: SudokuModel
    ,new : SudokuModel
}

easy = [ "9...2....",
         "7.1..4..8",
         ".32.7..4.",
         "...6.78..",
         ".8.....7.",
         "..65.1...",
         ".4..6.58.",
         "5..4..6.9",
         "....1...7"
      ]

medium = [".....67.."
         ,"....8.3.4"
         ,"..8....15"
         ,".3..64.2."
         ,"..5...9.."
         ,".8.57..4."
         ,"35....6.."
         ,"8.7.3...."
         ,"..17....."
         ]
hard = [ "..9.1...5"
        ,".8.65.9.."
        ,"......4.."
        ,".6......7"
        ,"4.1.7.3.9"
        ,"3......6."
        ,"..3......"
        ,"..8.32.4."
        ,"7...9.1.."
        ]
extreme = [ ".9.1.6.8."
           ,"5...9...7"
           ,".2.....9."
           ,"..6.8.9.."
           ,"...4.2..."
           ,"..4.3.1.."
           ,".4.....7."
           ,"3...7...9"
           ,".6.8.3.5."
           ]

charListToModel: (List String) -> SudokuModel
charListToModel lines =
    let
        charToCell c =
            if c == '.' then
               Possibles (Set.fromList [1..9])
           else
                case String.toInt (String.fromChar c) of
                    Ok value -> Filled value
                    Err error -> Bug
    in
       Matrix.fromList ( List.map (\line -> 
           List.map charToCell (String.toList line)
       ) lines)

init : Model
init =
    let 
        model = charListToModel extreme
    in
        {
            new = model
            , old = model
        }

subMatrix: (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
subMatrix loc size matrix = 
    let
        (r0,c0) = loc
        (rct, cct) = size
        slice list start size = List.take size (List.drop start list)
        rows = slice (Matrix.toList matrix) r0 rct
        newRows = List.map (\row -> slice row c0 cct) rows
    in
       Matrix.fromList newRows

extractColumn: Int -> Matrix a -> List a
extractColumn c m =
   if c < 0 || c >= (Matrix.colCount m) then
      []
  else
    List.filterMap ( \rowNr -> Matrix.get (rowNr, c) m) [0..(Matrix.rowCount m)]
        

-- return only the Filled values from a list
filledInValues: List Cell -> Set.Set Int
filledInValues list = 
    Set.fromList ( List.filterMap (\v -> 
        case v of
            Filled value -> Just value
            Possibles _ -> Nothing
            Bug -> Nothing
        ) list)

-- remove possibles from lines where there's a Filled value
-- in horizontal or vertical direction
removePossiblesFromLines: SudokuModel -> SudokuModel
removePossiblesFromLines model =
    let
        -- rowValues and columnValues are arrays of Set Int
        -- they represent for each row or column those values that are already picked
        rowValues: Array.Array (Set.Set Int)
        rowValues = Array.map (\rowArray -> filledInValues (Array.toList rowArray)) model
        columnValues: Array.Array (Set.Set Int)
        columnValues = Array.fromList ( List.map (\colNr -> 
            filledInValues  (extractColumn colNr model) 
        ) [0..((Matrix.colCount model)-1)])
        filter location el =
            let
                (r,c)=location
            in
               case el of
                   Filled _ ->
                       el
                   Possibles possibles ->
                       let
                           rowSet = Maybe.withDefault Set.empty (Array.get r rowValues)
                           colSet = Maybe.withDefault Set.empty (Array.get c columnValues)
                           allSet = Set.union rowSet colSet
                       in
                           Possibles (Set.diff possibles allSet)
                   Bug ->
                       Bug
    in
       Matrix.mapWithLocation filter model



-- See if there's only one location for a possible of a certain value on a row or column
-- update the model by filling in those values

handleSingleOnLine: SudokuModel -> SudokuModel
handleSingleOnLine model = 
     let
         coordModel = Matrix.mapWithLocation (,) model
         rowsAndColumns: List (List (Location, Cell))
         rowsAndColumns = Matrix.toList coordModel ++  List.map (\colNr ->
             extractColumn colNr coordModel
         ) [0..((Matrix.colCount coordModel)-1)]
         unFilleds: List (List (Location, Set.Set Int))
         unFilleds = 
             List.map ( \vec ->
                 List.filterMap ( \(loc, cell) ->
                     case cell of
                         Possibles possibles ->
                             Just (loc, possibles)
                         _ ->
                             Nothing

                 ) vec
             ) rowsAndColumns
         singles: Dict.Dict Location Int
         singles = Dict.fromList (List.foldl ( \vec accu ->
            (findSingles vec) ++ accu ) [] unFilleds)
     in
        Matrix.mapWithLocation (\location value ->
            case Dict.get location singles of
                Nothing ->
                    value
                Just single->
                    Filled single
        ) model

handlePairsInLine: Int -> SudokuModel -> SudokuModel
handlePairsInLine groupCt model =
     let



         coordModel = Matrix.mapWithLocation (,) model
         rowsAndColumns: List (List (Location, Cell))
         rowsAndColumns = Matrix.toList coordModel ++  List.map (\colNr ->
             extractColumn colNr coordModel
         ) [0..((Matrix.colCount coordModel)-1)] 

         vector' = \x -> vector groupCt x
         -- for example vectors: 
         --   [([(8,0),(8,2),(8,4),(8,6),(8,8)],[(Set.fromList [(8,0),(8,2)],Set.fromList [7,9])])]
         vectors: List ( List Location , List ( Set.Set Location, Set.Set Int ))
         vectors = List.filterMap vector' rowsAndColumns

     in
       modelUpdaterFromGroups model vectors

vector: Int -> List (Location, Cell) -> Maybe ( List (Location), List ((Set.Set (Location), Set.Set (Int))))
vector groupCt cells = 
     let 
         filleds: List (Location, Set.Set Int)
         filleds = List.filterMap ( \(loc, cell) ->
             case cell of
                 Possibles possibles ->
                     Just (loc, possibles)
                 _ ->
                     Nothing

         ) cells
         groupLists: List (List Location, Set.Set Int)
         groupLists = Utils.findMultiples' filleds groupCt |> Dict.toList 
         groupSets =  groupLists |> (List.map (\(k,v) -> ((Set.fromList k), v)))
         filledLocations: List Location
         filledLocations = List.map fst filleds 
     in
        case groupSets of
            [] -> Nothing
            _ -> Just (filledLocations, groupSets)

modelUpdaterFromGroups: SudokuModel -> List ( List Location , List ( Set.Set Location, Set.Set Int )) ->SudokuModel
modelUpdaterFromGroups model vectors = 
    let
         colupdater: List Location -> Set.Set Location -> Set.Set Int -> SudokuModel -> SudokuModel
         colupdater locations group groupValues m = 
             let
                 cellUpdater: Location -> Cell -> Cell
                 cellUpdater location cell =
                     let 
                         inGroup = Set.member location group 
                     in
                        if inGroup then
                           case cell of
                               Possibles values -> Possibles groupValues
                               _ -> Bug
                        else
                           case cell of
                               Possibles values -> cell
                               _ -> Bug
             in
                 List.foldl (\location accu ->
                     Matrix.update location (\cell ->
                         cellUpdater location cell
                    ) accu
                 ) m locations
         
         colupdater': List Location -> List (Set.Set Location, Set.Set Int ) -> SudokuModel -> SudokuModel
         colupdater' locations groups m =
             List.foldl (\(group, groupValues) accu ->
                 colupdater locations group groupValues accu
             ) m groups
     in   
        List.foldl (\(locations, groups) accu ->
             colupdater' locations groups accu
         ) model vectors 

handlePairsInSquare: Int -> SudokuModel -> SudokuModel
handlePairsInSquare groupCt model =
    let
        coordModel: Matrix (Location, Cell)
        coordModel = model |> (Matrix.mapWithLocation (,))

        squares = Matrix.square 3 (\(r,c) ->
                subMatrix (3*r, 3*c) (3,3) coordModel |> Matrix.flatten )
            |> Matrix.flatten 
        vector' = \x -> vector groupCt x
        vectors: List ( List Location , List ( Set.Set Location, Set.Set Int ))
        vectors = List.filterMap vector' squares

     in
       modelUpdaterFromGroups model vectors

-- remove possibles from Filled values in all 3x3 squares
removePossiblesFromSquares: SudokuModel -> SudokuModel
removePossiblesFromSquares model = 
    let
        subMatrices = Matrix.square 3 (\(r,c) ->
            let
                matrix3x3 = subMatrix (3*r, 3*c) (3,3) model
                values = filledInValues (Matrix.flatten matrix3x3)
            in
               (matrix3x3, values))
        filter location el =
            let
                (r,c)=location
            in
               case el of
                   Possibles possibles ->
                       case Matrix.get (r//3, c//3) subMatrices of
                            Just (_, values) ->
                               Possibles (Set.diff possibles values)
                            Nothing ->
                               Bug
                   _ ->
                       el
    in
       Matrix.mapWithLocation filter model


-- fill all cells with just one possible
handle1Possibles: SudokuModel -> SudokuModel
handle1Possibles model = 
   Matrix.map (\el ->
       case el of 
           Possibles x -> 
               case Set.toList x of
                   [y] -> Filled y
                   _ -> el
           _ -> el
       ) model


-- UPDATE

type Action = RemovePossiblesFromSquare | RemovePossiblesFromLines | Handle1Possibles | 
        HandleSingleOnLine | HandleSinglesInSquare | HandlePairsInSquare | HandleTripletsInSquare | 
        HandlePairsInLine | HandleTripletsInLine | Undo | Test

update : Action -> Model -> Model
update action model =
    let
        oldold = model.old
        old = .new model
        updater = 
          case (Debug.log "action" action) of
            RemovePossiblesFromSquare ->
              removePossiblesFromSquares old
            RemovePossiblesFromLines ->
              removePossiblesFromLines old
            Handle1Possibles ->
              old |> removePossiblesFromLines |> removePossiblesFromSquares |> handle1Possibles
            HandleSingleOnLine ->
              old |> removePossiblesFromLines |> removePossiblesFromSquares |> handleSingleOnLine
            HandleSinglesInSquare ->
              old |> removePossiblesFromLines |> removePossiblesFromSquares |> (handlePairsInSquare 1)
            HandlePairsInSquare ->
              old |> removePossiblesFromLines |> removePossiblesFromSquares |> (handlePairsInSquare 2)
            HandleTripletsInSquare ->
              old |> removePossiblesFromLines |> removePossiblesFromSquares |> (handlePairsInSquare 3)
            HandlePairsInLine ->
              old |> removePossiblesFromLines |> removePossiblesFromSquares |> (handlePairsInLine 2)
            HandleTripletsInLine ->
              old |> removePossiblesFromLines |> removePossiblesFromSquares |> (handlePairsInLine 3)
            Undo ->
                oldold
            Test ->
                let 
                    m=handleSingleOnLine old
                in
                  old
    in
       {
           old = old
           ,new = updater
       }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        new = .new model
        old = .old model
        rows new = List.map2 (,) (Matrix.toList old) (Matrix.toList new)
        oneCell (oldCell,cell) = 
            let
                possibles oldP newP =
                    Set.diff oldP newP |> Set.toList

                (highlight, lostPossibles) = 
                    if oldCell == cell  then
                       ("", [])
                   else
                        case (oldCell, cell) of 
                        (Possibles oldP, Possibles newP) -> ("", (possibles oldP newP))
                        (Possibles _, Filled _) -> ("changed", [])
                        _ -> ("", []) -- unchanged
            in
                span [class ("cell " ++ highlight)] [
                case cell of
                    Filled value ->
                        span [class "content Filled"] [
                            text ( toString value)
                        ]
                    Possibles possibles ->
                        span [class "content Possibles"]  ((
                        List.map (\p -> 
                            span [class ("Possible p" ++ (toString p))] [
                                text (toString p)
                            ]) (Set.toList possibles)) ++
                        List.map (\p -> 
                            span [class ("Possible highlight p" ++ (toString p))] [
                                text (toString p)
                            ]) lostPossibles)
                    Bug ->
                        span [class "content Bug"] [
                            text "BUG"
                        ]
                ]
        htmlRow (oldrow, row) = div [] (List.map oneCell ( List.map2 (,) oldrow row) )
        buttonLine (action,t) =
            li [] [ 
                span [] [text t]
                , button [ onClick address action ] [text "do it"] ]

    in
       div [] 
         [ div [class "sudoku"] (List.map htmlRow (rows new))

         , ol [] (List.map buttonLine [
               (RemovePossiblesFromSquare, "remove possibles from 3x3 squares")
             , (RemovePossiblesFromLines, "remove possibles from hor. and vert. lines")
             , (Handle1Possibles, "#1, #2 and fill 1 single possibles in 3x3 squares")
             , (HandleSingleOnLine, "#1, #2 and fill 1 single possible location in rows or columns")
             , (HandleSinglesInSquare, "#1, #2 and find singles in a square")
             , (HandlePairsInSquare, "#1, #2 and find pairs in a square")
             , (HandleTripletsInSquare, "#1, #2 and find triplets in a square")
             , (HandlePairsInLine, "#1, #2 and find pairs in a line")
             , (HandleTripletsInLine, "#1, #2 and find triplets in a line")
             , (Test, "testing..")
             , (Undo, "undo..")
             ] )
         ]


