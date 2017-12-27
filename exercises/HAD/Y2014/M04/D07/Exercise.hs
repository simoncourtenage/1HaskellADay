module HAD.Y2014.M04.D07.Exercise where

{- | braid
   Braid two lists

   Examples:

   >>> braid [0,2] [1,3]
   [0,1,2,3]

   >>> braid [0,2] [1,3 ..] 
   [0,1,2,3]

   >>> braid [0,2 ..] [1,3]
   [0,1,2,3]
-}
braid :: Ord a => [a] -> [a] -> [a]
braid [] ys = []
braid xs [] = []
braid xss@(x:xs) yss@(y:ys) | x <= y    = x : braid xs yss
                            | otherwise = y : braid xss ys
