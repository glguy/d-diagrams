module Main (main) where

import Ersatz (Result(Satisfied), anyminisat, solveWith)
import Prelude hiding (all, (&&), (||), not, any, and, or)
import Puzzle (Puzzle(Puzzle), Elt(M, O, C), makePuzzle)
import Rendering (renderPuzzle)
import Solution (solutionExists)

-----------------------------------------------------------------------

example :: Puzzle ()
example = Puzzle
      [5,6,3,2,5,5,3,5]
  [(5,[o,o,o,o,o,o,o,o])
  ,(2,[o,M,o,o,o,o,o,M])
  ,(7,[o,o,o,o,o,o,o,o])
  ,(4,[o,o,M,o,o,M,o,M])
  ,(2,[M,o,o,o,o,o,o,o])
  ,(4,[o,o,o,o,o,o,o,o])
  ,(4,[o,o,o,o,o,o,M,o])
  ,(6,[o,o,o,M,o,o,o,o])
  ]
  where
    o = O ()

example2 :: Puzzle ()
example2 = Puzzle
       [1,4,2,7,0,4,4,4]
  [(3, [o,o,o,o,o,o,o,o])
  ,(2, [o,o,o,o,o,o,o,M])
  ,(5, [o,o,M,o,o,o,o,o])
  ,(3, [o,o,o,o,o,o,o,M])
  ,(4, [o,o,o,o,o,o,o,o])
  ,(1, [o,C,o,o,o,o,o,M])
  ,(4, [o,o,o,o,o,o,o,o])
  ,(4, [o,o,o,o,o,o,o,M])
  ]
  where
    o = O ()

example3 :: Puzzle Bool
example3 = makePuzzle
  [[w,o,o,o,w,o,o,o]
  ,[o,o,w,o,w,C,o,o]
  ,[o,w,w,o,w,o,o,o]
  ,[o,o,w,o,w,w,o,w]
  ,[w,o,w,o,M,w,o,o]
  ,[o,o,w,w,w,w,w,o]
  ,[o,w,w,o,o,o,w,o]
  ,[o,o,o,o,w,o,o,o]
  ]
  where
    w = O False
    o = O True

-- | Print out all the solutions to a puzzle.
run :: Puzzle a -> IO ()
run = go []
  where
    go old p =
     do res <- solveWith anyminisat (solutionExists old p)
        case res of
          (Satisfied, Just q) ->
           do putStrLn (renderPuzzle q)
              go (q:old) p
          _ -> pure ()

main :: IO ()
main =
 do run example
    run example2
    run example3
