module Rendering (renderPuzzle) where

import Data.Map qualified as Map
import Ersatz (false)
import Puzzle (Puzzle, Elt(O, M, C), clueRows, topClues, isPath, rows)
import Regions (cardinal)
import System.Console.ANSI (setSGRCode, SGR(..), ConsoleIntensity(..), Color(..), ConsoleLayer(..), ColorIntensity(..))
import Utils (gridMap)

renderPuzzle :: Puzzle Bool -> String
renderPuzzle p = unlines
  $ (setSGRCode [SetConsoleIntensity BoldIntensity] <>
     " " <> concatMap show (topClues p) <>
     setSGRCode [Reset])
  : [ setSGRCode [SetConsoleIntensity BoldIntensity] <>
      show n <>
      setSGRCode [Reset] <>
      concat [char (y,x) e | (x,e) <- zip [0..] row]
    | (y,(n,row)) <- zip [0..] (clueRows p)]
  where
    m = gridMap (rows p)

    path k = maybe false isPath (Map.lookup k m)

    char k = \case
      M       -> setSGRCode [SetColor Foreground Dull Red   ] ++ 'M' : setSGRCode [Reset]
      C       -> setSGRCode [SetColor Background Dull Yellow,
                             SetColor Foreground Dull Black ] ++ 'C' : setSGRCode [Reset]
      O False -> setSGRCode [SetColor Foreground Dull Green ] ++ '·' : setSGRCode [Reset]
      O True  -> setSGRCode [SetColor Foreground Dull Blue  ] ++ c   : setSGRCode [Reset]
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
