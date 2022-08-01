{-# Language ScopedTypeVariables #-}
{- |
Module:       Main
Description:  Read a puzzle and print its solution.
-}
module Main (main) where

import Control.Monad (unless)
import Data.ByteString.Lazy qualified as B
import Data.Foldable (for_, traverse_)
import Data.List.Split ( chunksOf )
import Ersatz (Result(..), solveWith, anyminisat, dimacsSAT)
import Parser (parse, parseSolutionArray)
import Prelude hiding (all, (&&), (||), not, any, and, or)
import Puzzle (Puzzle)
import Rendering (printSolutions)
import Solution (solutionExists, validateSolution)
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )

-- | Print out all the solutions to a puzzle.
solveAll :: Puzzle a -> IO [Puzzle Bool]
solveAll = go []
  where
    go old p =
     do res <- solveWith anyminisat (solutionExists old p)
        case res of
          (Satisfied, Just q) -> go (q:old) p
          _ -> pure old

-- | Print out all the solutions to a puzzle.
check1 :: Puzzle Bool -> IO ()
check1 p =
 do valid <- validateSolution p
    if valid then
     do res <- solveWith anyminisat (solutionExists [p] p)
        case res of
          (Satisfied, Just q) ->
           do printSolutions [p,q]
              putStrLn "Not Unique!"
              exitFailure
          (Unsatisfied, _) -> pure ()
          _ ->
           do putStrLn "Broken!"
              exitFailure
    else
     do printSolutions [p]
        putStrLn "Bad input puzzle"
        exitFailure

-- | Main entry point
main :: IO ()
main =
 do args <- getArgs
    case args of
      ["check"] -> checkMode
      ["solve"] -> solveMode
      ["dimacs", outfile] -> dimacsMode outfile
      ["dimacs-unique", outfile] -> dimacsUniqueMode outfile
      _         -> usage

checkMode :: IO ()
checkMode =
 do input <- getContents

    ps <-
      case parseSolutionArray input of
        Nothing ->
         do hPutStrLn stderr "Failed to parse input"
            exitFailure
        Just xs -> pure xs
    
    for_ (chunksOf 8 ps) \chunk ->
     do traverse_ check1 chunk
        printSolutions chunk

solveMode :: IO ()
solveMode =
 do input <- getContents

    p <-
      case parse input of
        Just p -> pure p
        Nothing ->
         do hPutStrLn stderr "Failed to parse input"
            exitFailure

    slns <- solveAll p
    case slns of
      [y] -> printSolutions [y]
      [] -> putStrLn "No solutions"
      _ ->
       do putStrLn "Begin ambiguous solutions"
          for_ (chunksOf 8 slns) printSolutions
          putStrLn "End ambiguous solutions"

dimacsMode :: FilePath -> IO ()
dimacsMode outfile =
 do input <- getContents

    p <-
      case parse input of
        Just p -> pure p
        Nothing ->
         do hPutStrLn stderr "Failed to parse input"
            exitFailure

    B.writeFile outfile (dimacsSAT (solutionExists [] p))


dimacsUniqueMode :: FilePath -> IO ()
dimacsUniqueMode outfile =
 do input <- getContents

    p <-
      case parseSolutionArray input of
        Just [p] -> pure p
        _ ->
         do hPutStrLn stderr "Failed to parse input"
            exitFailure

    valid <- validateSolution p
    unless valid
     do putStrLn "Invalid puzzle"
        exitFailure

    B.writeFile outfile (dimacsSAT (solutionExists [p] p))

usage :: IO ()
usage =
 do hPutStrLn stderr "Usage: d-diagrams <check|solve|dimacs|dimacs-unique>"
    exitFailure
