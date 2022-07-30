{-# LANGUAGE MonadComprehensions #-}
{- |
Module:       Solution
Description:  Functions for puzzle solutions.
-}
module Solution (solutionExists, validPuzzle) where

import Prelude hiding (all, (&&), (||), not, any, and, or)
import Data.Map qualified as Map
import Data.Traversable (for)
import Ersatz
    ( assert,
      exists,
      Bit,
      Boolean((&&), (==>), (||), false, not, any, all),
      Codec(encode),
      Equatable((/==)),
      MonadSAT )
import Puzzle
    ( Puzzle(clueRows),
      Elt(C, O, M),
      rows,
      isPath,
      isChest,
      clueCols )
import Grids (cardinal, gridMap, region3, twoArc, walls3)
import Counting (exactly, atLeast)

-- | Predicate for solved puzzles.
validPuzzle :: Boolean a => Puzzle a -> a
validPuzzle p = validCounts p && validCells p

-- | Predicate for puzzles with valid row and column count.
validCounts :: Boolean a => Puzzle a -> a
validCounts p =
  all (\(n,xs) -> exactly n (not . isPath <$> xs)) (clueRows p <> clueCols p)

-- | Predicate for puzzles with valid local element constraints.
validCells :: Boolean a => Puzzle a -> a
validCells p = all (uncurry validCell) (Map.assocs m)
  where
    validCell k = \case
      O o -> o ==> inChestRoom k || atLeast 2 (entrances cardinal k) && no2x2 k
      M   -> exactly 1 (entrances cardinal k)
      C   -> inChestRoom k

    m = gridMap (rows p)
    at k = Map.lookup k m

    inChestRoom = any (\x -> Map.findWithDefault false x cache) . region3
      where
        cache = Map.mapMaybeWithKey (\k _ -> isChestRoomCenter k) m
        isChestRoomCenter k =
          [ all isPath room && exactly 1 (entrances walls3 k)        
            | room <- traverse at (region3 k)
            , exactly 1 (isChest <$> room)
          ]

    entrances f k = [isPath z | Just z <- at <$> f k]

    no2x2 c = not (any (all isPath) (traverse at (twoArc c)))

-- | Generate a solution to the input puzzle.
solutionExists ::
  MonadSAT s m =>
  [Puzzle Bool] {- ^ excluded solutions -} ->
  Puzzle a      {- ^ puzzle to solve    -} ->
  m (Puzzle Bit)
solutionExists old p =
 do b <- for p \_ -> exists
    assert (validPuzzle b && all (\o -> encode o /== b) old)
    return b
