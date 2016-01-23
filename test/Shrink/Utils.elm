module Shrink.Utils where

import Shrink exposing (Shrinker, array)
import Matrix exposing (Matrix)

matrix : Shrinker a -> Shrinker (Matrix a)
matrix = array >> array
