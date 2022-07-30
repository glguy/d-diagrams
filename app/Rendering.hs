{- |
Module:       Rendering
Description:  Functions for rendering solved puzzles.
-}
module Rendering (printSolution) where

import Data.Map qualified as Map
import Ersatz (false)
import Grids (gridMap, cardinal)
import Puzzle (Puzzle, Elt(O, M, C), clueRows, topClues, isPath, rows)
import System.Console.ANSI
import Data.Foldable (for_)

-- | Print a solution to the terminal using ANSI formatting.
printSolution :: Puzzle Bool -> IO ()
printSolution p =
 do setSGR [SetConsoleIntensity BoldIntensity]
    putStr (" " <> concatMap show (topClues p))
    setSGR [Reset]
    putStrLn ""

    for_ (zip [0..] (clueRows p)) \(y,(n,row)) ->
     do setSGR [SetConsoleIntensity BoldIntensity]
        putStr (show n)
        setSGR [Reset]
        
        for_ (zip [0..] row) \(x,e) ->
          elt (y,x) e
        putStrLn ""
  where
    m = gridMap (rows p)

    path k = maybe false isPath (Map.lookup k m)

    elt k = \case
      M ->
       do setSGR [SetColor Foreground Dull Red]
          putChar 'M'
          setSGR [Reset]
      C ->
       do setSGR [SetColor Background Dull Yellow, SetColor Foreground Dull Black]
          putChar 'C'
          setSGR [Reset]
      O False ->
       do setSGR [SetColor Foreground Dull Green]
          putChar '·'
          setSGR [Reset]
      O True  ->
       do setSGR [SetColor Foreground Dull Blue]
          putChar c
          setSGR [Reset]
        where
          c =
            case path <$> cardinal k of
                [False,False,True ,True ] -> '┌'
                [False,True ,False,True ] -> '┐'
                [False,True ,True ,False] -> '─'
                [False,True ,True ,True ] -> '┬'
                [True ,False,False,True ] -> '│'
                [True ,False,True ,False] -> '└'
                [True ,False,True ,True ] -> '├'
                [True ,True ,False,False] -> '┘'
                [True ,True ,False,True ] -> '┤'
                [True ,True ,True ,False] -> '┴'
                [True ,True ,True ,True ] -> '┼'
                _                         -> '!'
