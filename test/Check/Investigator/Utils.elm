module Check.Investigator.Utils where

import Check.Investigator exposing (..)
import Shrink.Utils as S
import Shrink exposing (noShrink)

import Random 

import Utils exposing (..)

-- dictOfList: Investigator a -> Investigator b -> Investigator (DictOfList a b)
-- dictOfList loc =
--   let
--     keyCt = rangeInt 0 9
--     listCt = rangeInt 0 9
--     valueCt = rangeInt 1 9
--   in
--     investigator
--       ( randomDictOfList keyCt.generator listCt.generator valueCt loc.generator)
--       (S.matrix v.shrinker)

location : Investigator Location
location =
  tuple (rangeInt 0 8, rangeInt 0 8)

randomDictOfList Generator Int -> Generator Int -> Generator Int -> Generator a -> Generator (DictOfList a Int)
randomDictOfList keyCtGenerator listGenerator valueGenerator keyGenerator =



  widthGenerator `andThen` \width ->
    heightGenerator `andThen` \height ->
      let
        rowGenerator = 
          list width elementGenerator
          |> map Array.fromList
        
        matrixAsArrayGenerator = 
          list height rowGenerator
          |> map Array.fromList 
        
      in
        matrixAsArrayGenerator
