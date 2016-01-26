module Utils.Matrix where

import Matrix exposing (..)
import List

subMatrix: (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
subMatrix loc size matrix = 
    let
        (r0,c0) = loc
        (rct, cct) = size
        slice list start size = List.take size (List.drop start list)
        rows = slice (Matrix.toList matrix) r0 rct
        newRows = List.map (\row -> slice row c0 cct) rows
    in
       Matrix.fromList newRows

extractColumn: Int -> Matrix a -> List a
extractColumn c m =
   if c < 0 || c >= (Matrix.colCount m) then
      []
  else
    List.filterMap ( \rowNr -> Matrix.get (rowNr, c) m) [0..(Matrix.rowCount m)]
