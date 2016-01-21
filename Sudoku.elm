module Sudoku where

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Matrix exposing (..)


-- MODEL

type Cell = Filled Int | Possibles (List Int)
type alias Model = Matrix (Cell)


init : Model
init = Matrix.square 9 (\loc -> 
    let
        (r,c) = loc
    in
       if r==c then
          Filled (r + 1)
       else
          Possibles [(r+1)..(c+1)]
      )




-- UPDATE

type Action = Increment |Decrement

update : Action -> Model -> Model
update action model =
  case action of
    Increment ->
      model 
    Decrement ->
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
            ]
        htmlRow row = div [] (List.map oneCell row)
    in
      div [class "sudoku"] (List.map htmlRow (rows model))


