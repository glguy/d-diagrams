{- |
Module:       Main
Description:  Read a puzzle and print its solution.
-}
module Main (main) where

import Ersatz (Result(Satisfied), solveWith, anyminisat)
import Prelude hiding (all, (&&), (||), not, any, and, or)
import Puzzle (Puzzle)
import Rendering (printSolution)
import Solution (solutionExists)
import Parser (parse)

-- | Print out all the solutions to a puzzle.
run :: Puzzle a -> IO ()
run = go []
  where
    go old p =
     do res <- solveWith anyminisat (solutionExists old p)
        case res of
          (Satisfied, Just q) ->
           do printSolution q
              go (q:old) p
          _ -> putStrLn "No more solutions"

-- | Main entry point
main :: IO ()
main =
 do input <- getContents
    case parse input of
      Nothing -> putStrLn "Failed to parse input"
      Just p -> run p
