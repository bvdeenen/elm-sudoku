
module UtilsTest where

import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Investigator.Utils exposing (..)
import Maybe as M exposing (..)
import Debug

import Utils

tests = suite "Utils test suite"
  [ -- claim_to_list_and_from_list_are_inverses
  ]

-- claim_to_list_and_from_list_are_inverses =
--   claim
--     "reverseMap and reverseMap' are inverses"
--   `that`
--     (Utils.reverseMap >> Utils.reverseMap')
--   `is`
--     identity
--   `for`
--     dictOfList location
