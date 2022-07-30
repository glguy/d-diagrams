{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Puzzle (
  Puzzle(..),
  Elt(..),
  isPath,
  isChest,
  puzzleMap,
  clueCols,
  rows,
  makePuzzle,
) where

import Data.List (transpose)
import Data.Map (Map)
import Ersatz (Boolean, true, false, bool, Equatable, (===), Codec(..))
import GHC.Generics (Generic)
import Utils (gridMap)

-----------------------------------------------------------------------
-- Puzzle grid element representation
-----------------------------------------------------------------------

data Elt a
  = M -- ^ monster
  | C -- ^ chest
  | O a -- ^ true is open path, false is wall
  deriving (Read, Show, Traversable, Foldable, Functor, Generic, Equatable, Eq, Ord)

isPath :: Boolean b => Elt b -> b
isPath = \case
  O o -> o
  _   -> true

isChest :: Boolean b => Elt a -> b
isChest = \case
  C -> true
  _ -> false

-----------------------------------------------------------------------
-- Puzzle representation
-----------------------------------------------------------------------

data Puzzle a = Puzzle {
  topClues :: [Int],           -- ^ column clues
  clueRows :: [(Int, [Elt a])] -- ^ row clues and rows
  }
  deriving (Read, Show, Traversable, Foldable, Functor, Generic, Equatable, Eq, Ord)

instance Codec a => Codec (Puzzle a) where
  type Decoded (Puzzle a) = Puzzle (Decoded a)
  decode = traverse . decode
  encode = fmap encode

rows :: Puzzle a -> [[Elt a]]
rows = map snd . clueRows

cols :: Puzzle a -> [[Elt a]]
cols = transpose . rows

puzzleMap :: Puzzle a -> Map (Int,Int) (Elt a)
puzzleMap = gridMap . rows

clueCols :: Puzzle a -> [(Int, [Elt a])]
clueCols p = zip (topClues p) (cols p)

-- | Build a puzzle with row and column clues given a solution.
makePuzzle :: [[Elt Bool]] -> Puzzle Bool
makePuzzle solution =
  Puzzle {
    topClues = clue <$> transpose solution,
    clueRows = [(clue row, row) | row <- solution] }
  where
    clue = length . filter (not . isPath)

instance Equatable Int where
  x === y = bool (x == y)
