module Utils where

import Data.Map (Map)
import Data.Map qualified as Map

gridMap :: [[a]] -> Map (Int,Int) a
gridMap zss = 
  Map.fromList
    [ ((y,x), z)
      | (y, zs) <- zip [0..] zss
      , (x, z ) <- zip [0..] zs
    ]
