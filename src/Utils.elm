
module Utils where

import Dict
import Set
import Debug

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




findMultiples: List (comparable', Set.Set comparable) -> Dict.Dict (List comparable') (Set.Set comparable)
findMultiples tupleList =
    let
        groups = List.map (\(val,set) ->
            (val, Set.singleton (Set.toList set))
        ) (reverseMap tupleList |> Dict.toList) 

        groupedPositions: Dict.Dict (List comparable) (Set.Set comparable')
        groupedPositions = reverseMap groups 
    in
        Dict.filter (\positions values ->
            (List.length positions) == (Set.size values)
        ) groupedPositions


findMultiples': List (comparable', Set.Set comparable) -> Int -> Dict.Dict (List comparable') (Set.Set comparable)
findMultiples' tupleList ct =
    findMultiples tupleList |> Dict.filter (\pos _ -> (List.length pos) == ct)



combineListOfDicts dicts = 
    List.foldl (\dict acc -> Dict.union dict acc) Dict.empty  dicts
