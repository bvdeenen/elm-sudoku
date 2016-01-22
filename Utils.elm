
module Utils where

import Dict

dictUpdate: comparable -> a -> (Dict.Dict comparable (List a)) -> (Dict.Dict comparable (List a))
dictUpdate key val dict =
    Dict.update key (\m ->
        case m of 
            Nothing ->
                Just [val]
            Just values ->
                Just (val :: values) ) dict
                
dictUpdate': comparable -> (List a) -> (Dict.Dict comparable (List a)) -> (Dict.Dict comparable (List a))

dictUpdate' key vals dict =
    List.foldl (\val accu ->
        dictUpdate key val accu
        ) dict vals

reverseMap:  List (a, List comparable) -> Dict.Dict comparable (List a)
reverseMap tupleList =
    List.foldl (\(loc, values) accu ->
        List.foldl (\val accu' ->
            dictUpdate val loc accu'
        ) accu values
    ) Dict.empty tupleList

findSingles:  List (a, List comparable) -> List (a, comparable)
findSingles tupleList =
    Dict.foldl (\val locations accu ->
        case locations of 
            [loc] ->
                (loc, val) :: accu
            _ ->
                accu) [] (reverseMap tupleList) 





