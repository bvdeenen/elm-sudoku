
module Utils where

import Dict
import Set

dictUpdate: comparable -> comparable' -> (Dict.Dict comparable (Set.Set comparable')) 
    -> (Dict.Dict comparable (Set.Set comparable'))
dictUpdate key val dict =
    Dict.update key (\m ->
        case m of 
            Nothing ->
                Just (Set.singleton val)
            Just values ->
                Just (Set.insert val values )) dict
                

reverseMap:  List (comparable', Set.Set comparable) -> Dict.Dict comparable (Set.Set comparable')
reverseMap tupleList =
    List.foldl (\(loc, values) accu ->
        Set.foldl (\val accu' ->
            dictUpdate val loc accu'
        ) accu values
    ) Dict.empty tupleList

findSingles:  List (comparable', Set.Set comparable) -> List (comparable', comparable)
findSingles tupleList =
    Dict.foldl (\val locations accu ->
        case Set.toList locations of 
            [loc] ->
                (loc, val) :: accu
            _ ->
                accu) [] (reverseMap tupleList) 





