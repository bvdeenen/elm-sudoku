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

type Cell = Filled Int | Possibles (List Int) | Bug
type alias Model = Matrix (Cell)

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

charListToModel: (List String) -> Model
charListToModel lines =
    let
        charToCell c =
            if c == '.' then
               Possibles [1..9]
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
    charListToModel extreme

subMatrix: (Int,Int) -> (Int,Int) -> Model -> Model
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
        

filledInValues: List Cell -> Set.Set Int
filledInValues list = 
    Set.fromList ( List.filterMap (\v -> 
        case v of
            Filled value -> Just value
            Possibles _ -> Nothing
            Bug -> Nothing
        ) list)

removePossiblesFromLines: Model -> Model
removePossiblesFromLines model =
    let
        filledInValues' array = filledInValues (Array.toList array)
        -- rowValues and columnValues are arrays of Set Int
        rowValues = Array.map (\rowArray -> filledInValues' rowArray) model
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
                           possiblesSet = Set.fromList possibles
                           rowSet = Maybe.withDefault Set.empty (Array.get r rowValues)
                           colSet = Maybe.withDefault Set.empty (Array.get c columnValues)
                           allSet = Set.union rowSet colSet
                       in
                           Possibles (Set.toList (Set.diff possiblesSet allSet))
                   Bug ->
                       Bug
    in
       Matrix.mapWithLocation filter model




handleSingleOnLine: Model -> Model
handleSingleOnLine model = 
     let
         coordModel = Matrix.mapWithLocation (,) model
         rowsAndColumns: List (List (Location, Cell))
         rowsAndColumns = Matrix.toList coordModel ++  List.map (\colNr ->
             extractColumn colNr coordModel
         ) [0..((Matrix.colCount coordModel)-1)]
         unFilleds: List (List (Location, List Int))
         unFilleds = 
             (Debug.watch "unFilleds" (List.map ( \vec ->
                 List.filterMap ( \(loc, cell) ->
                     case cell of
                         Possibles possibles ->
                             Just (loc, possibles)
                         _ ->
                             Nothing

                 ) vec
             ) rowsAndColumns))
         singles = (Debug.log "singles" (Dict.fromList (List.foldl ( \vec accu ->
            (findSingles vec) ++ accu ) [] unFilleds)))
     in
        Matrix.mapWithLocation (\location value ->
            case Dict.get location singles of
                Nothing ->
                    value
                Just single->
                    Filled single
        ) model

removePossiblesFromSquares: Model -> Model
removePossiblesFromSquares model = 
    let
        subMatrices = Matrix.square 3 (\(r,c) ->
            let
                pos = (3*r, 3*c)
                matrix3x3 = subMatrix pos (3,3) model
                values = filledInValues (Matrix.flatten matrix3x3)
            in
               (matrix3x3, values))
        filter location el =
            let
                (r,c)=location
            in
               case el of
                   Possibles possibles ->
                       let
                           possiblesSet = Set.fromList possibles
                       in
                           case Matrix.get (r//3, c//3) subMatrices of
                                Just (_, values) ->
                                   Possibles (Set.toList (Set.diff possiblesSet values))
                                Nothing ->
                                   Bug
                   _ ->
                       el
    in
       Matrix.mapWithLocation filter model


handle1Possibles: Model -> Model
handle1Possibles model = 
   Matrix.map (\el ->
       case el of 
           Filled _ ->
               el
           Possibles [x] ->
               Filled x
           Possibles _ ->
               el
           Bug ->
               Bug
       ) model


-- UPDATE

type Action = RemovePossiblesFromSquare | RemovePossiblesFromLines | Handle1Possibles | 
        HandleSingleOnLine | Test

update : Action -> Model -> Model
update action model =
  case (Debug.log "action" action) of
    RemovePossiblesFromSquare ->
      removePossiblesFromSquares model
    RemovePossiblesFromLines ->
      removePossiblesFromLines model
    Handle1Possibles ->
      model |> removePossiblesFromLines |> removePossiblesFromSquares |> handle1Possibles
    HandleSingleOnLine ->
      model |> removePossiblesFromLines |> removePossiblesFromSquares |> handleSingleOnLine
    Test ->
        let 
            m = handleSingleOnLine model
        in
          model



-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        rows model = Matrix.toList model
        oneCell cell = 
            span [class "cell"] [
            case cell of
                Filled value ->
                    span [class "content Filled"] [
                        text ( toString value)
                    ]
                Possibles possibles ->
                    span [class "content Possibles"]  (
                    List.map (\p -> 
                        span [class ("Possible p" ++ (toString p))] [
                            text (toString p)
                        ]) possibles)
                Bug ->
                    span [class "content Bug"] [
                        text "BUG"
                    ]
            ]
        htmlRow row = div [] (List.map oneCell row)
    in
       div [] 
         [ div [class "sudoku"] (List.map htmlRow (rows model))
         , button [ onClick address RemovePossiblesFromSquare ] [ text "#" ]
         , button [ onClick address RemovePossiblesFromLines ] [ text "L" ]
         , button [ onClick address Handle1Possibles ] [ text "1" ]
         , button [ onClick address HandleSingleOnLine ] [ text "1L" ]
         , button [ onClick address Test ] [ text "Test" ]
         ]


