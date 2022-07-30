{-# LANGUAGE ParallelListComp #-}
module Counting where

import Prelude hiding (not, or)
import Ersatz (Boolean(choose, false, not, or, true))

-- | Count the number of true bits a list.
--
-- Each element of the infinite output list will be set true iff
-- the index of that output bit corresponds to the count of true
-- bits in the input list.
counts :: Boolean b => [b] -> [b]
counts = foldr addBit (true : repeat false)
  where
    addBit x ns = [choose n np1 x | n <- ns | np1 <- false : ns]

-- | Inclusive lower bound exclusive upper bound count test.
inRange :: Boolean b => Int -> Int -> [b] -> b
inRange lo hi = or . take (hi-lo) . drop lo . counts

exactly :: Boolean b => Int -> [b] -> b
exactly n = inRange n (n+1)

fewerThan :: Boolean b => Int -> [b] -> b
fewerThan = inRange 0 

atLeast :: Boolean b => Int -> [b] -> b
atLeast n = not . fewerThan n

atMost :: Boolean b => Int -> [b] -> b
atMost n = fewerThan (n+1)

moreThan :: Boolean b => Int -> [b] -> b
moreThan n = atLeast (n+1)
