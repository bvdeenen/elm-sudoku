
module UtilsTest where

import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Investigator.Utils exposing (..)
import Maybe as M exposing (..)
import Debug
import Matrix exposing (Matrix, Location)

import Utils
import Dict
import Set

tests = suite "Utils test suite"
  [ 
      claim_findSingles_finds_values_that_exist_only_once
  ]

claim_findSingles_finds_values_that_exist_only_once =
  claim
    "findSingles partitions the tuple list correctly"
  `true`
    ( \tuples -> 
        let
            singles: Set.Set (Int,Int)
            singles  = Utils.findSingles tuples  |> List.sort |> Set.fromList 
            allValues: List (Int)
            allValues = List.foldl (\(key, values) accu ->
                Set.union accu values
            ) Set.empty tuples |> Set.toList
            locationsMap: List (Int, List(Int))
            locationsMap = List.map (\value ->
                (value, List.filterMap ( \(key, values) ->
                    case Set.member value values of 
                        True -> Just key
                        False -> Nothing
                ) tuples)
            ) allValues
            valuesOnOnlyOneLocation: Set.Set (Int,Int)
            valuesOnOnlyOneLocation = List.filterMap (\(value, locations) ->
                case locations of
                    [loc] -> Just (loc, value)
                    _ -> Nothing
                ) locationsMap |> List.sort |> Set.fromList 
            compare = (Debug.log "values" valuesOnOnlyOneLocation) == (Debug.log "singles" singles) 
            |> (Debug.log "compare")
        in
           valuesOnOnlyOneLocation == singles

    )
  `for`
    dictOfList 


location : Investigator Location
location =
  tuple (rangeInt -10 50, rangeInt -10 50)
