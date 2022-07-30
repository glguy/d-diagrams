module Regions (
    cardinal,
    region3,
    walls3,
    twoArc,
) where

-----------------------------------------------------------------------
-- Region shapes
-----------------------------------------------------------------------

cardinal :: (Int, Int) -> [(Int, Int)]
cardinal (y,x) =
  [          (y-1,x),
    (y,x-1), {- c -} (y,x+1),
             (y+1,x)]

region3 :: (Int,Int) -> [(Int,Int)]
region3 (y,x) =
  [(y-1,x-1),(y-1,x),(y-1,x+1)
  ,(y  ,x-1),(y  ,x),(y  ,x+1)
  ,(y+1,x-1),(y+1,x),(y+1,x+1)]

walls3 :: (Int,Int) -> [(Int,Int)]
walls3 (y,x) =
  [          (y-2,x-1),(y-2,x),(y-2,x+1)
  ,(y-1,x-2),                           (y-1,x+2)
  ,(y  ,x-2),          {- c -}          (y  ,x+2)
  ,(y+1,x-2),                           (y+1,x+2)
  ,          (y+2,x-1),(y+2,x),(y+2,x+1)
  ]

twoArc :: (Int,Int) -> [(Int,Int)]
twoArc (y,x) =
  [{- c -} (y  ,x+1)
  ,(y+1,x),(y+1,x+1)]
