module Check.Investigator.Utils where

import Check.Investigator exposing (..)
--import Shrink.Utils as S
import Shrink exposing (..)

import Random 
import Set
import Dict

import Utils exposing (..)

dictOfList =
    investigator
      randomDictOfList
      noShrink

randomDictOfList = 
   let 
        generateOuter =
          (Random.int 0 9) `Random.andThen` (\i -> Random.list i generateInner)
        generateInner =
          (Random.pair (Random.int  0 9) ((Random.int 1 9) `Random.andThen` (\i -> Random.list i (Random.int 0 9))))
        convertToSet tuples = List.map ( \(key,values) ->
            (key, (Set.fromList values))
        ) tuples |> Dict.fromList |> Dict.toList
   in
      Random.map convertToSet generateOuter
