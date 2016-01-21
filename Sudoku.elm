module Sudoku where

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Set 
import String


-- MODEL

type Cell = Filled Int | Possibles (List Int)
type alias Model = Matrix (Cell)

easy = [ "..48.96.."
      , "8.1...2.5"
      , "6..7.5..3"
      , "..3.4.5.."
      , ".5.....5."
      , "..8.9.1.."
      , "9..2.4..7"
      , "1.5...9.8"
      , "..79.15.."
      ]

charListToModel: (List String) -> Model
charListToModel lines =
    let
        charToCell c =
            if c == '.' then
               Possibles [1..9]
           else
                case String.toInt (String.fromChar c) of
                    Ok value -> 
                        Filled value
                    Err error ->
                        Possibles [999]
    in
       Matrix.fromList ( List.map (\line -> 
           List.map charToCell (String.toList line)
       ) lines)





init : Model
init =
    charListToModel easy


init2 : Model
init2 = Matrix.square 9 (\loc -> 
    let
        (r,c) = loc
    in
       if r==c then
          Filled (r + 1)
       else
          Possibles [1..9]
      )


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

removePossiblesFromSquares: Model -> Model
removePossiblesFromSquares model = 
    let
        subMatrices = Matrix.square 3 (\loc ->
            let
                (r,c)=loc
                pos = (3*r, 3*c)
                matrix3x3 = subMatrix pos (3,3) model
                values = List.filterMap (\v -> 
                    case v of
                        Filled value -> Just value
                        Possibles _ -> Nothing
                    ) (Matrix.flatten matrix3x3)
            in
               (matrix3x3, (Set.fromList values))
           )
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
                       in
                           case Matrix.get (r//3, c//3) subMatrices of
                                Just (_, values) ->
                                   Possibles (Set.toList (Set.diff possiblesSet values))
                                Nothing ->
                                   Possibles [99]




    in
       Matrix.mapWithLocation filter model



-- UPDATE

type Action = RemovePossiblesFromSquare

update : Action -> Model -> Model
update action model =
  case action of
    RemovePossiblesFromSquare ->
      removePossiblesFromSquares model


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
            ]
        htmlRow row = div [] (List.map oneCell row)
    in
       div [] 
         [ div [class "sudoku"] (List.map htmlRow (rows model))
         , button [ onClick address RemovePossiblesFromSquare ] [ text "#" ]
         ]


