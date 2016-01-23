module Check.Investigator.Utils where

import Check.Investigator exposing (..)
--import Shrink.Utils as S
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

-- location : Investigator Location
-- location =
--   tuple (rangeInt 0 8, rangeInt 0 8)

randomDictOfList = 

   let 
        generateOuter =
          (Random.int 0 9) `Random.andThen` (\i -> Random.list i generateInner)
            
        generateInner =
          (Random.pair (Random.int  0 9) ((Random.int 1 9) `Random.andThen` (\i -> Random.list i (Random.int 0 3))))
   in
      generateOuter
