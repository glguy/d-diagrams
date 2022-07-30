{- |
Module:       Parser
Description:  Functions for parsing puzzles.
-}
module Parser (parse) where

import Data.Char ( isDigit, digitToInt )
import Puzzle (Puzzle(Puzzle), Elt(..))

-- | Parse a simple ASCII puzzle representation.
--
-- * @C@ for chests
-- * @M@ for monsters
-- * @.@ for empty spaces
--
-- @
--  0002
-- 1....
-- 0.C.M
-- 1....
-- @
parse :: String -> Maybe (Puzzle ())
parse input =
  case lines input of
    (' ':ns):rs ->
      Puzzle
        <$> traverse parseDigit ns
        <*> traverse parseRow rs
    _ -> Nothing

parseDigit :: Char -> Maybe Int
parseDigit c
  | isDigit c = Just $! digitToInt c
  | otherwise = Nothing

parseRow :: String -> Maybe (Int, [Elt ()])
parseRow (n:xs) = (,) <$> parseDigit n <*> traverse parseCell xs
parseRow [] = Nothing

parseCell :: Char -> Maybe (Elt ())
parseCell = \case
  '.' -> Just (O ())
  'C' -> Just C
  'M' -> Just M
  _   -> Nothing